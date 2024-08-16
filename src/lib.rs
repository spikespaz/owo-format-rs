use proc_macro::TokenStream as TokenStream1;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::parse::{Parse, ParseStream, Parser};
use syn::punctuated::{Pair, Punctuated};
use syn::{parse_quote, Expr, Path, Token};

pub(crate) fn proc_macro_impl(tokens2: impl FnOnce() -> syn::Result<TokenStream2>) -> TokenStream1 {
    tokens2()
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

#[proc_macro]
pub fn format_args_colored(tokens: TokenStream1) -> TokenStream1 {
    proc_macro_impl(|| {
        let segments = FormatSeq::parse_terminated.parse(tokens)?;
        let mut format_str = String::new();
        let mut format_args = Punctuated::<Expr, Token![,]>::new();
        for (styled, punct) in segments.into_pairs().map(Pair::into_tuple) {
            match punct {
                Some(FormatPunct::Comma(_)) => format_str.push_str("{} "),
                Some(FormatPunct::Semi(_)) => format_str.push_str("{}\n"),
                None | Some(FormatPunct::None) => format_str.push_str("{}"),
            }
            let display = styled.into_expr();
            format_args.push(parse_quote!(#display));
        }
        Ok(quote! {
            format_args!(#format_str, #format_args)
        })
    })
}

type FormatSeq = Punctuated<ExprOpsSeq, FormatPunct>;

struct ExprOpsSeq {
    ops: Vec<(Token![:], Path)>,
    expr: Expr,
}

enum FormatPunct {
    Comma(#[allow(dead_code)] Token![,]),
    Semi(#[allow(dead_code)] Token![;]),
    None,
}

impl ExprOpsSeq {
    fn into_expr(self) -> Expr {
        let expr = &self.expr;
        let ops = self.ops.iter().map(|(_, path)| path);
        parse_quote!( #expr #( .#ops() )* )
    }
}

impl From<ExprOpsSeq> for Expr {
    fn from(value: ExprOpsSeq) -> Self {
        value.into_expr()
    }
}

impl Parse for ExprOpsSeq {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut ops = Vec::new();
        while !input.is_empty() {
            if input.peek(Token![:]) {
                ops.push((input.parse::<Token![:]>().unwrap(), input.parse::<Path>()?));
            } else {
                break;
            }
        }
        let expr = input
            .parse::<Expr>()
            .map_err(|e| syn::Error::new(e.span(), "expected an expression or a format string"))?;
        Ok(Self { ops, expr })
    }
}

impl Parse for FormatPunct {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![,]) {
            Ok(Self::Comma(input.parse().unwrap()))
        } else if input.peek(Token![;]) {
            Ok(Self::Semi(input.parse().unwrap()))
        } else {
            Ok(Self::None)
        }
    }
}
