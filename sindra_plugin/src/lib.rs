#![feature(plugin_registrar, rustc_private)]

extern crate syntax;
extern crate rustc_plugin;
extern crate rustc_data_structures;

use std::collections::HashMap;

use syntax::codemap::{Span, Spanned};
use syntax::parse::common::SeqSep;
use syntax::parse::token;
use syntax::symbol::Symbol;
use syntax::parse::parser::PathStyle;
use syntax::ast::{self, Ident, Mac_};
use syntax::tokenstream::{ThinTokenStream, TokenStream, TokenTree};
use syntax::ext::base::{ExtCtxt, MacResult, DummyResult, MacEager};
use syntax::ext::build::AstBuilder;
use rustc_plugin::Registry;
use rustc_data_structures::small_vec::SmallVec;

macro_rules! err {
    ($sp:ident, $result:expr) => (
        match $result {
            Ok(value) => value,
            Err(mut bldr) => {
                bldr.emit();
                return DummyResult::any(bldr.span.primary_span().unwrap_or($sp));
            }
        }
    )
}

macro_rules! lparen   { () => (token::OpenDelim( token::DelimToken::Paren  )) }
macro_rules! rparen   { () => (token::CloseDelim(token::DelimToken::Paren  )) }
macro_rules! lbracket { () => (token::OpenDelim( token::DelimToken::Bracket)) }
macro_rules! rbracket { () => (token::CloseDelim(token::DelimToken::Bracket)) }

fn expand(cx: &mut ExtCtxt, sp: Span, args: &[TokenTree]) -> Box<MacResult + 'static> {

    let mut tt_parser = cx.new_parser_from_tts(args);

    // first part of input is the target macro, followed by a comma
    let macro_path = err!(sp, tt_parser.parse_path(PathStyle::Type));
    err!(sp, tt_parser.expect(&token::Comma));

    // parse the list of elements in the target macro and their possible default values,
    // followed by a comma
    let mut defaults: HashMap<Symbol, Vec<TokenTree>> = HashMap::new();
    let element_names = err!(sp, tt_parser.parse_seq(&lbracket![], &rbracket![],
            SeqSep { sep: Some(token::Comma), trailing_sep_allowed: true }, |parser| {

        let element_name = match parser.parse_ident() {
            Ok(value) => { value },
            Err(e) => { return Err(e); }
        };
        if parser.token == lparen!() {
            parser.bump();
            let mut tts: Vec<TokenTree> = vec![];
            while parser.token != rparen!() && parser.token != token::Eof {
                tts.push(parser.parse_token_tree());
            }
            defaults.insert(element_name.name, tts);
            match parser.expect(&rparen!()) {
                Ok(_) => {},
                Err(e) => { return Err(e); },
            }
        }
        Ok(element_name)
    }));
    err!(sp, tt_parser.expect(&token::Comma));

    // parse the element specifications
    let mut elements: HashMap<Symbol, Vec<TokenTree>> = HashMap::new();
    while let token::Ident(element_name) = tt_parser.token {
        tt_parser.bump();
        err!(sp, tt_parser.expect(&token::Colon));

        let mut tts: Vec<TokenTree> = vec![];
        while tt_parser.token != token::Comma && tt_parser.token != token::Eof {
            let token_tree = tt_parser.parse_token_tree();
            tts.push(token_tree);
        }
        tt_parser.eat(&token::Comma);
        elements.insert(element_name.name, tts);
    }

    // construct the output macro call and return
    let Spanned { node: element_names, .. } = element_names;
    let mut tts = match construct_macro_call(sp, element_names, elements, defaults) {
        Ok(tts) => { tts },
        Err(e) => {
            cx.span_err(sp, &e);
            return DummyResult::any(sp);
        }
    };
    let mac = Mac_ {
        path: macro_path,
        tts: ThinTokenStream::from(
            TokenStream::concat(tts.drain(..).map(|tt| TokenStream::from(tt)).collect())
        ),
    };
    let spanned_mac = Spanned { node: mac, span: sp };
    MacEager::items(SmallVec::one(
        cx.item(sp, Ident::from_str(""), vec![], ast::ItemKind::Mac(spanned_mac))))
}

fn construct_macro_call<'a>(
    sp: Span,
    element_names: Vec<Ident>,
    elements: HashMap<Symbol, Vec<TokenTree>>,
    defaults: HashMap<Symbol, Vec<TokenTree>>,
) -> Result<Vec<TokenTree>, String> {

    let mut tts: Vec<TokenTree> = vec![];
    for element_name in element_names {
        // check if specified, or if in defaults
        let mut elem_tts = match elements.get(&element_name.name)
                .or_else(|| defaults.get(&element_name.name)) {
            Some(tts) => { tts.clone() },
            None => {
                return Err(format!("element '{}' not specified and no default exists",
                    element_name.name));
            }
        };
        tts.push(TokenTree::Token(sp, token::Ident(element_name)));
        tts.push(TokenTree::Token(sp, lparen!()));
        tts.append(&mut elem_tts);
        tts.push(TokenTree::Token(sp, rparen!()));
    }
    Ok(tts)
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_macro("macro_preparse", expand);
}
