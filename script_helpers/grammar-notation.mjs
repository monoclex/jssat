import fs from "fs";
import fetch from "node-fetch";
import { parse } from "node-html-parser";
import {
  Parser,
  EmitFormat,
  Production,
  RightHandSide,
  RightHandSideList,
} from "grammarkdown";

try {
  fs.mkdirSync("workdir");
} catch {}

// 1. fetch `spec.html` if it does not exist

if (!fs.existsSync("workdir/spec.html"))
  await fetch("https://github.com/tc39/ecma262/blob/es2021/spec.html?raw=true")
    .then((resp) => resp.text())
    .then((text) => {
      fs.writeFileSync("workdir/spec.html", text);
    });

// 2. grab all grammar notation sections
const spec = fs.readFileSync("workdir/spec.html", { encoding: "utf-8" });

const grammarSections = parse(spec)
  .getElementsByTagName("emu-grammar")
  .filter((element) => element.attributes["type"] === "definition")
  .map((element) => element.innerText);

// 3. parse all grammar sections
const parser = new Parser();

/** @type {Production[]} */
const productions = grammarSections
  .map((grammar) => parser.parseSourceFile("...", grammar))
  .flatMap((file) => file.elements)
  .filter((sourceElement) => sourceElement instanceof Production);

const ast = productions
  .filter(({ body }) => body instanceof RightHandSideList)
  .map(({ name, parameterList: prodParamList, body: prodBody }) => {
    /** @type {Parameter[]} */
    const parameters = (prodParamList && prodParamList.elements) ?? [];

    /** @type {RightHandSide[]} */
    const body = (prodBody && prodBody.elements) ?? [];

    return {
      name: name.text,
      parameters: parameters.map((parameter) => parameter.name),
      body: body.map((rhsList) => rhsList),
    };
  });

// 4. save AST to JSON to then give to rust code for code generation
fs.writeFileSync("workdir/grammar-notation.json", JSON.stringify(ast, null, 4));
