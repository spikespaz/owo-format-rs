use proc_macro::TokenStream as TokenStream1;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::parse::{Parse, ParseStream, Parser};
use syn::punctuated::{Pair, Punctuated};
use syn::token::Paren;
use syn::{
    parenthesized, parse_quote, AngleBracketedGenericArguments, Expr, ExprLit, ExprMethodCall,
    Ident, Lit, LitStr, Token,
};

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
                None | Some(FormatPunct::Concat) => format_str.push_str("{}"),
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
    pub ops: Vec<(Token![:], FormatOp)>,
    pub expr: FormatExpr,
}

enum FormatOp {
    Method(TraitMethod),
    MethodCall(TraitMethodCall),
}

struct TraitMethod {
    // pub attrs: Vec<Attribute>,
    pub method: Ident,
    pub turbofish: Option<AngleBracketedGenericArguments>,
}

struct TraitMethodCall {
    // pub attrs: Vec<Attribute>,
    pub method: Ident,
    pub turbofish: Option<AngleBracketedGenericArguments>,
    pub paren_token: Paren,
    pub args: Punctuated<Expr, Token![,]>,
}

enum FormatExpr {
    Format(LitStr),
    Verbatim(Expr),
}

enum FormatPunct {
    Comma(#[allow(dead_code)] Token![,]),
    Semi(#[allow(dead_code)] Token![;]),
    Concat,
}

impl FormatSeg {
    fn into_expr(self) -> Expr {
        let expr = match self.expr {
            FormatExpr::Format(format) => parse_quote!(format_args!(#format)),
            FormatExpr::Verbatim(expr) => expr,
        };
        self.ops.into_iter().fold(expr, |acc, (_, op)| {
            Expr::MethodCall(op.into_call_with_expr(acc))
        })
    }
}

impl FormatOp {
    fn into_call_with_expr(self, expr: Expr) -> ExprMethodCall {
        match self {
            Self::Method(method) => ExprMethodCall {
                attrs: Vec::new(),
                receiver: Box::new(expr),
                dot_token: <Token![.]>::default(),
                method: method.method,
                turbofish: method.turbofish,
                paren_token: Paren::default(),
                args: Punctuated::new(),
            },
            Self::MethodCall(method) => ExprMethodCall {
                attrs: Vec::new(),
                receiver: Box::new(expr),
                dot_token: <Token![.]>::default(),
                method: method.method,
                turbofish: method.turbofish,
                paren_token: method.paren_token,
                args: method.args,
            },
        }
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
                ops.push((input.parse().unwrap(), input.parse()?));
            } else {
                break;
            }
        }
        let expr = input.parse()?;
        Ok(Self { ops, expr })
    }
}

impl Parse for FormatOp {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let method = input.parse()?;
        let turbofish = if input.peek(Token![::]) {
            Some(AngleBracketedGenericArguments::parse_turbofish(input)?)
        } else {
            None
        };
        if input.peek(Paren) {
            let content;
            Ok(Self::MethodCall(TraitMethodCall {
                method,
                turbofish,
                paren_token: parenthesized!(content in input),
                args: content.parse_terminated(Expr::parse, Token![,])?,
            }))
        } else {
            Ok(Self::Method(TraitMethod { method, turbofish }))
        }
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
            Ok(Self::Concat)
        }
    }
}
