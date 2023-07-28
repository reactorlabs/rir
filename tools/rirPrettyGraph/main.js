// printPrettyGraph.cpp puts all elements as divs using HTML attributes as if they would be rendered in a pure HTML.
// However, we actually use cytoscape, which has its own rendering mechanisms.
// So we essentially use HTML as a fancy DSL to store our element data,
// and this script parses that fancy DSL using the DOM API and translates it into cytoscape's DSL.

// Get fallback and input
const sourcesNeeded = document.getElementById("sources-needed");
const input = document.getElementsByName("main")[0];

// Create Cytoscape container and sidebar
const container = document.createElement("main");
container.id = "cy";
const sidebar = document.createElement("aside");
sidebar.id = "sidebar";
const nameDiv = document.createElement("div");
nameDiv.id = "name";
const bodyDiv = document.createElement("div");
bodyDiv.id = "body";
sidebar.appendChild(nameDiv);
sidebar.appendChild(bodyDiv);

// Translate input into output
// See https://js.cytoscape.org/#getting-started/including-cytoscape.js and the rest of the API
// for an overview of how the cytoscape DSL works
// noinspection JSUnresolvedReference
const output = {
    container,
    elements: [],
    style
};
const elementsWithParents = new Map();
for (const child of input.children) {
    let name = child.getElementsByClassName("name")[0].innerHTML;
    output.elements.push({
        group: "nodes",
        data: {
            id: child.id,
            name,
            label: child.id in name ? name : `${name}\n(${child.id})`,
            body: child.getElementsByClassName("body")[0]?.innerHTML,
        },
        classes: child.className,
    });
    for (const connected of child.getElementsByClassName("arrow")) {
        const target = connected.getAttribute("data-connected");
        output.elements.push({
            group: "edges",
            data: {
                label: connected.innerHTML,
                source: child.id,
                target
            },
            classes: connected.className,
        })
        if (connected.hasAttribute("data-is-child")) {
            if (target in elementsWithParents) {
                console.warn("Multiple parents for " + target + "!");
            }
            // A bit confusing: child is actually the parent here, and target is its child
            // `child` refers to input.children
            elementsWithParents.set(target, child.id);
        }
    }
}
for (const [element, parent] of elementsWithParents.entries()) {
    output.elements.find(e => e.data.id === element).data.parent = parent;
}

// Remove fallback and input, add container and sidebar
document.removeChild(sourcesNeeded);
document.removeChild(input);
document.appendChild(container);
document.appendChild(sidebar)

// Create cytoscape graph
// noinspection JSUnresolvedReference
const graph = cytoscape(output)