import { visit } from "unist-util-visit";
import { is } from "unist-util-is";

export default function remarkEpigraph({ className = "epigraph", headerTags = ["h1", "h2", "h3"] } = {}) {
    return (tree) => {
        visit(tree, ({ tagName }) => headerTags.includes(tagName), (node, index, parent) => {
            let n = 0;
            while (
                is(parent.children[index + 1 + n], { type: "text", value: "\n" })
                && is(parent.children[index + 2 + n], { tagName: "blockquote" })
            ) n += 2;
            if (n == 0) return;

            parent.children.splice(index + 2, n, {
                type: "element",
                tagName: "div",
                properties: { class: "epigraph" },
                children: parent.children.slice(index + 2, index + 2 + n),
            });
        });
    };
}