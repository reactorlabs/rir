// printPrettyGraph.cpp puts all elements as divs using HTML attributes as if they would be rendered in a pure HTML.
// However, we actually use cytoscape, which has its own rendering mechanisms.
// So we essentially use HTML as a fancy DSL to store our element data,
// and this script parses that fancy DSL using the DOM API and translates it into cytoscape's DSL.

// Get fallback and input
const sourcesNeeded = document.getElementById("sources-needed");
const input = document.getElementById("js-input");

// Create Cytoscape container and sidebar
const container = document.createElement("main");
container.id = "cy";
const sidebar = document.createElement("aside");
sidebar.id = "sidebar";
const addrDiv = document.createElement("div");
addrDiv.id = "addr";
const bodyDiv = document.createElement("div");
bodyDiv.id = "body";
sidebar.appendChild(addrDiv);
sidebar.appendChild(bodyDiv);

// Translate input into output
// See https://js.cytoscape.org/#getting-started/including-cytoscape.js and the rest of the API
// for an overview of how the cytoscape DSL works
// noinspection JSUnresolvedReference
const output = {
    container,
    elements: [],
    style,
    layout: {
        name: "fcose",
        nodeRepulsion: 100000,
        idealEdgeLength: 300,
    },
};
const elementsWithParents = new Map();
for (const child of input.children) {
    output.elements.push({
        group: "nodes",
        data: {
            id: child.id,
            name: child.getElementsByClassName("name").item(0)?.innerHTML,
            body: child.getElementsByClassName("body").item(0)?.innerHTML,
        },
        classes: child.className,
    });
    for (const connected of child.getElementsByClassName("arrow")) {
        const target = connected.getAttribute("data-connected");
        if (connected.hasAttribute("data-is-child")) {
            if (target in elementsWithParents) {
                console.warn("Multiple parents for " + target + "!");
            }
            // A bit confusing: child is actually the parent here, and target is its child
            // `child` refers to input.children
            elementsWithParents.set(target, child.id);
        } else {
            output.elements.push({
                group: "edges",
                data: {
                    label: connected.innerHTML,
                    source: child.id,
                    target
                },
                classes: connected.className,
            })
        }
    }
}
for (const [element, parent] of elementsWithParents.entries()) {
    output.elements.find(e => e.data.id === element).data.parent = parent;
}

// Remove fallback and input, add container and sidebar
document.body.removeChild(sourcesNeeded);
// Don't actually remove so we can inspect the source, just hide
input.display = "none";
document.body.appendChild(container);
document.body.appendChild(sidebar)

// Create cytoscape graph
// noinspection JSUnresolvedReference
const graph = cytoscape(output)