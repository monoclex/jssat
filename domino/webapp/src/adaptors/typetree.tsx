import { Accessor } from "solid-js";
import { Moment, MomentRecordKey, MomentValue, MomentValues } from "../api";
import { DisplayTreeNode, TreeNode, TreeViewProps } from "../panels/TreeView";

export function adaptTypeTree(
  moment: Accessor<Moment | undefined>
): () => TreeViewProps | undefined {
  return () => {
    const now = moment();
    if (!now) return undefined;

    const values = now.values;
    return {
      tree: Object.keys(now.values.registers).map((register) =>
        nodeRegister(values, register)
      ),
    };
  };
}

function nodeRegister(values: MomentValues, register: string): TreeNode {
  const value = values.registers[register];

  return {
    Display: () => (
      <span>
        %{register} = <DisplayValue values={values} value={value} />
      </span>
    ),
  };
}

interface DisplayValueProps {
  value: MomentValue;
  values: MomentValues;
}

function DisplayValue(props: DisplayValueProps) {
  const value = props.value;
  const values = props.values;

  // prettier-ignore
  switch (value.kind) {
    case "atom":  return `atom(${value.atom})`;
    case "bool":  return `bool(${value.bool})`;
    case "bytes": return `bytes(${value.bytes})`;
    case "fnptr": return `fn(${value.fnptr})`;
    case "num":   return `num(${value.num})`;
    case "list":  return <DisplayList   values={values} id={value.list} />;
    case "rec":   return <DisplayRecord values={values} id={value.rec}  />;
  }
}

interface DisplayListProps {
  values: MomentValues;
  id: string;
}

function DisplayList(props: DisplayListProps) {
  const list = props.values.lists[props.id];
  const name = `list(len = ${list.length})`;

  return (
    <span>
      {name}
      {list.length !== 0 && (
        <DisplayTreeNode treeNode={nodeList(props.values, list)} />
      )}
    </span>
  );
}

function nodeList(values: MomentValues, list: MomentValue[]): TreeNode {
  return {
    expand: () =>
      list.map((value) => ({
        Display: () => {
          return <DisplayValue values={values} value={value} />;
        },
      })),
  };
}

interface DisplayRecordProps {
  values: MomentValues;
  id: string;
}

function DisplayRecord(props: DisplayRecordProps) {
  const record = props.values.records[props.id];
  const name = `rec(len = ${record.length}, id = ${props.id})`;

  return (
    <span>
      {name}
      {record.length !== 0 && (
        <DisplayTreeNode treeNode={nodeRecord(props.values, record)} />
      )}
    </span>
  );
}

function nodeRecord(
  values: MomentValues,
  pairs: [MomentRecordKey, MomentValue][]
): TreeNode {
  return {
    expand: () =>
      pairs.map(([key, value]) => ({
        Display: () => {
          return (
            <span>
              <DisplayValue values={values} value={key} />
              {" ↦ "}
              <DisplayValue values={values} value={value} />
            </span>
          );
        },
      })),
  };
}
