import { createSignal, For, JSX, Show } from "solid-js";

export type TreeNode =
  | { Display: () => JSX.Element }
  | { expand: () => TreeNode[] };

export interface TreeViewProps {
  tree: TreeNode[];
}

export function TreeView(props: TreeViewProps) {
  return (
    <ul className="type-list">
      <For each={props.tree}>
        {(treeNode) => (
          <li>
            <DisplayTreeNode treeNode={treeNode} />
          </li>
        )}
      </For>
    </ul>
  );
}

interface DisplayTreeNodeProps {
  treeNode: TreeNode;
}

export function DisplayTreeNode(props: DisplayTreeNodeProps) {
  const treeNode = props.treeNode;

  if ("Display" in treeNode) {
    return <treeNode.Display />;
  } else {
    const [show, setShow] = createSignal(false);

    return (
      <Show
        when={show()}
        fallback={
          <span className="clickable" onClick={() => setShow(true)}>
            ...expand...
          </span>
        }
      >
        {() => <TreeView tree={treeNode.expand()} />}
      </Show>
    );
  }
}
