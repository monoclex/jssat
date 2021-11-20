mod exprs;
mod stmts;

use swc_common::{
    errors::{ColorConfig, Handler},
    input::StringInput,
    sync::Lrc,
    FileName, SourceMap,
};
use swc_ecmascript::parser::{PResult, Parser, Syntax};

use super::parse_nodes as js;
use swc_ecmascript::ast as swc;

pub fn parse_script(script: &str) -> PResult<js::Script> {
    let swc_script = to_swc_script(script.to_string())?;
    Ok(swc_script.to_parse_node())
}

fn to_swc_script(source: String) -> PResult<swc::Script> {
    // https://github.com/Starlight-JS/starlight/blob/4c4ce5d0178fb28c3b2a044d572473baaf057b73/crates/starlight/src/vm.rs#L275-L300
    let cm: Lrc<SourceMap> = Default::default();
    let handler = Handler::with_tty_emitter(ColorConfig::Auto, true, false, Some(cm.clone()));
    let fm = cm.new_source_file(FileName::Anon, source);

    let mut parser = Parser::new(
        Syntax::Es(Default::default()),
        StringInput::from(fm.as_ref()),
        None,
    );

    for err in parser.take_errors() {
        err.into_diagnostic(&handler).emit();
    }

    parser.parse_script()
}

trait ToParseNode<P> {
    fn to_parse_node(self) -> P;
}

impl ToParseNode<js::Script> for swc::Script {
    fn to_parse_node(self) -> js::Script {
        if self.body.is_empty() {
            return js::Script::Variant0(None);
        }

        let script_body = js::ScriptBody::Variant0(self.body.to_parse_node().into());

        js::Script::Variant0(Some(script_body.into()))
    }
}
