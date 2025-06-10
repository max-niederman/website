import { visit } from "unist-util-visit";

export function remarkTypedLinks() {
    return function (tree, _) {
        visit(tree, "link", (node) => {
            let type = typeForLink(new URL(node.url, "https://maxniederman.com"));
            if (type) {
                node.data ??= {};
                node.data.hProperties ??= {};
                node.data.hProperties["data-link-type"] = type;
            }
        });
    };
}

/** Determine the link type based on its URL.
 * @param {URL} url
 * @returns {string | null}
*/
function typeForLink(url) {
    switch (url.protocol) {
        case "mailto:":
            return "email";
        case "tel:":
            return "phone";
        case "https:":
        case "http:":
            switch (url.hostname.replace(/^www\./, "")) {
                case "maxniederman.com":
                    return "internal";
                case "en.wikipedia.org":
                    return "wikipedia";
                case "github.com":
                    return "github";
                case "linkedin.com":
                    return "linkedin";
                case "discord.com":
                case "discord.gg":
                case "discordapp.com":
                    return "discord";
                case "nytimes.com":
                    return "nytimes";
                default:
                    return null;
            }
    }
}