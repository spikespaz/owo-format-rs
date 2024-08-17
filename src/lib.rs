use proc_macro::TokenStream as TokenStream1;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::parse::{Parse, ParseStream, Parser};
use syn::punctuated::{Pair, Punctuated};
use syn::{parse_quote, Expr, ExprLit, Lit, LitStr, Path, Token};

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
            format_args.push(styled.into_expr());
        }
        Ok(quote! {
            format_args!(#format_str, #format_args)
        })
    })
}

type FormatSeq = Punctuated<FormatSeg, FormatPunct>;

struct FormatSeg {
    ops: Vec<(Token![:], Path)>,
    expr: FormatExpr,
}

enum FormatExpr {
    Format(LitStr),
    Verbatim(Expr),
}

enum FormatPunct {
    Comma(#[allow(dead_code)] Token![,]),
    Semi(#[allow(dead_code)] Token![;]),
    None,
}

impl FormatSeg {
    fn into_expr(self) -> Expr {
        let ops = self.ops.iter().map(|(_, path)| path);
        let expr = match self.expr {
            FormatExpr::Format(format) => parse_quote!(format_args!(#format)),
            FormatExpr::Verbatim(expr) => expr,
        };
        parse_quote!( #expr #( .#ops() )* )
    }
}

impl From<FormatSeg> for Expr {
    fn from(value: FormatSeg) -> Self {
        value.into_expr()
    }
}

impl Parse for FormatSeg {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut ops = Vec::new();
        while !input.is_empty() {
            if input.peek(Token![:]) {
                ops.push((input.parse::<Token![:]>().unwrap(), input.parse::<Path>()?));
            } else {
                break;
            }
        }
        let expr = input.parse()?;
        Ok(Self { ops, expr })
    }
}

// TODO: attrs are silently removed
impl Parse for FormatExpr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        match input.parse::<Expr>()? {
            Expr::Lit(ExprLit {
                attrs: _, // can the format literal have attributes?
                lit: Lit::Str(format),
            }) => Ok(Self::Format(format)),
            expr => Ok(Self::Verbatim(expr)),
        }
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
