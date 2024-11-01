import { visit } from "unist-util-visit";
import { toString } from "mdast-util-to-string";
import GithubSlugger from "github-slugger";

/** @import { HeadingTree, Heading } from "./heading-tree" */
/** @import * as mdast from "mdast" */

const slugger = new GithubSlugger();

export function remarkHeadingTree() {
	return function (tree, { data }) {
		data.astro.frontmatter.headingTree = buildHeadingTree(tree);
	};
}

/** Build a tree of headings from an mdast tree, also adding heading IDs where they are missing.
 * @param {mdast.Root} tree
 * @returns {HeadingTree[]}
 */
function buildHeadingTree(tree) {
	const stack = [{ data: { depth: 0 }, children: [] }];

	visit(tree, "heading", (node) => {
		while (node.depth <= stack[stack.length - 1].data.depth) {
			stack.pop();
		}

		let parent = stack[stack.length - 1];
		parent.children.push({ data: extractHeadingData(node), children: [] });

		stack.push(parent.children[parent.children.length - 1]);
	});

	return stack[0].children;
}

/** Extract data from a heading, adding an ID if necessary.
 * @param {mdast.Heading} node
 * @returns {Heading}
 */
function extractHeadingData(node) {
  const contentString = toString(node);

  node.data ??= {};
  node.data.hProperties ??= {};
  node.data.hProperties.id ??= slugger.slug(contentString);

	return {
		id: node.data.hProperties.id,
		depth: node.depth,
		content: contentString,
	};
}
