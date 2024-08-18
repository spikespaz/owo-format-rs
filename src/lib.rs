use proc_macro::TokenStream as TokenStream1;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream, Parser};
use syn::punctuated::{Pair, Punctuated};
use syn::spanned::Spanned as _;
use syn::token::{Brace, Paren};
use syn::{
    braced, parenthesized, parse_quote, AngleBracketedGenericArguments, Expr, ExprLit,
    ExprMethodCall, ExprPath, Ident, Lit, LitStr, Token,
};

pub(crate) fn proc_macro_impl<T: ToTokens>(
    tokens2: impl FnOnce() -> syn::Result<T>,
) -> TokenStream1 {
    tokens2()
        .map(ToTokens::into_token_stream)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

#[proc_macro]
pub fn format_args_colored(tokens: TokenStream1) -> TokenStream1 {
    proc_macro_impl(|| {
        FormatSeq::parse_terminated
            .parse(tokens)
            .and_then(FormatArgs::try_from)
    })
}

type FormatSeq = Punctuated<FormatSeg, FormatPunct>;

struct FormatArgs {
    pub literal: String,
    pub args: Punctuated<Expr, Token![,]>,
}

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
    #[allow(dead_code)]
    Seq {
        dot: Token![.],
        brace: Brace,
        seq: FormatSeq,
    },
    Verbatim(Expr),
}

enum FormatPunct {
    Comma(#[allow(dead_code)] Token![,]),
    Semi(#[allow(dead_code)] Token![;]),
    Concat,
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

impl TraitMethod {
    pub fn parse_args<'p>(self, input: impl Into<ParseStream<'p>>) -> syn::Result<TraitMethodCall> {
        let input = input.into();
        let TraitMethod { method, turbofish } = self;
        let content;
        Ok(TraitMethodCall {
            method,
            turbofish,
            paren_token: parenthesized!(content in input),
            args: content.parse_terminated(Expr::parse, Token![,])?,
        })
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
        let method = input.parse::<TraitMethod>()?;

        if input.peek(Paren) {
            let ahead = input.fork();
            let _content;
            let _paren = parenthesized!(_content in ahead);
            // skipped the parenthesized item
            #[allow(clippy::if_same_then_else)]
            if ahead.is_empty() {
                Ok(FormatOp::Method(method))
            } else if let Some((punct, _cursor)) = ahead.cursor().punct() {
                match punct.as_char() {
                    ':' | ',' | ';' => Ok(FormatOp::Method(method)),
                    '.' => Ok(FormatOp::MethodCall(method.parse_args(input)?)),
                    _ => Ok(FormatOp::MethodCall(method.parse_args(input)?)),
                }
            } else if ahead.peek(Paren) {
                Err(ahead.error(
                    "parentheses are ambiguous in this position, \
                consider using alternative delimiters or additional punctuation",
                ))
            } else {
                Ok(FormatOp::MethodCall(method.parse_args(input)?))
            }
        } else {
            Ok(FormatOp::Method(method))
        }
    }
}

impl Parse for TraitMethod {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            method: input.parse()?,
            turbofish: if input.peek(Token![::]) {
                Some(AngleBracketedGenericArguments::parse_turbofish(input)?)
            } else {
                None
            },
        })
    }
}

// TODO: attrs are silently removed
impl Parse for FormatExpr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![.]) && input.peek2(Brace) {
            let content;
            Ok(Self::Seq {
                dot: input.parse()?,
                brace: braced!(content in input),
                seq: FormatSeq::parse_terminated(&content)?,
            })
        } else {
            match input.parse::<Expr>()? {
                Expr::Lit(ExprLit {
                    attrs: _, // can the format literal have attributes?
                    lit: Lit::Str(format),
                }) => Ok(Self::Format(format)),
                Expr::Path(ExprPath { path, .. }) if path.get_ident().is_some() => {
                    Err(syn::Error::new(
                        path.span(),
                        "bare identifiers are disallowed, please reference or wrap with an expression",
                    ))
                }
                Expr::Call(expr_call) => match expr_call.func.as_ref() {
                    Expr::Path(_) => Ok(Self::Verbatim(expr_call.into())),
                    _ => Err(syn::Error::new(
                        expr_call.func.span(),
                        "expression calls are not supported here",
                    )),
                },
                expr => Ok(Self::Verbatim(expr)),
            }
        }
    }
}

impl Parse for FormatPunct {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.is_empty() {
            // If there is a case that this is parsed with the correct function,
            // but this branch is entered anyway, this panic can be replaced with an error.
            panic!(
                "punctuation should have been parsed by syn::Punctuated::parse_terminated, \
                which does not expect trailing punctuation"
            )
        } else if input.peek(Token![,]) {
            Ok(Self::Comma(input.parse().unwrap()))
        } else if input.peek(Token![;]) {
            Ok(Self::Semi(input.parse().unwrap()))
        } else {
            // Not parsing one of the tokens means
            // that another syntax item follows.
            // This should have panicked at EOI,
            // so something *must* follow to concatenate.
            Ok(Self::Concat)
        }
    }
}

impl ToTokens for FormatArgs {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let FormatArgs { literal, args } = self;
        tokens.extend(quote!(format_args!(#literal, #args)))
    }
}

impl TryFrom<FormatSeq> for FormatArgs {
    type Error = syn::Error;

    fn try_from(segments: FormatSeq) -> syn::Result<Self> {
        let mut format_str = String::new();
        let mut format_args = Punctuated::<Expr, Token![,]>::new();
        for (styled, punct) in segments.into_pairs().map(Pair::into_tuple) {
            match punct {
                Some(FormatPunct::Comma(_)) => format_str.push_str("{} "),
                Some(FormatPunct::Semi(_)) => format_str.push_str("{}\n"),
                None | Some(FormatPunct::Concat) => format_str.push_str("{}"),
            }
            format_args.push(styled.try_into()?);
        }
        Ok(Self {
            literal: format_str,
            args: format_args,
        })
    }
}

impl TryFrom<FormatSeg> for Expr {
    type Error = syn::Error;

    fn try_from(segment: FormatSeg) -> syn::Result<Self> {
        let expr = match segment.expr {
            FormatExpr::Format(format) => parse_quote!(format_args!(#format)),
            FormatExpr::Seq { seq, .. } => {
                Expr::Verbatim(FormatArgs::try_from(seq)?.into_token_stream())
            }
            FormatExpr::Verbatim(expr) => expr,
        };
        Ok(segment.ops.into_iter().fold(expr, |acc, (_, op)| {
            Expr::MethodCall(op.into_call_with_expr(acc))
        }))
    }
}
