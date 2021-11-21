mod exprs;
mod stmts;

use std::convert::TryInto;

use swc_common::{input::StringInput, BytePos};
use swc_ecmascript::parser::{PResult, Parser};

use super::parse_nodes as js;
use swc_ecmascript::ast as swc;

pub fn parse_script(script: &str) -> PResult<js::Script> {
    let swc_script = to_swc_script(script)?;
    Ok(swc_script.to_parse_node())
}

fn to_swc_script(source: &str) -> PResult<swc::Script> {
    let mut parser = Parser::new(
        Default::default(),
        StringInput::new(
            source,
            BytePos(0),
            BytePos(source.len().try_into().unwrap()),
        ),
        None,
    );

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
