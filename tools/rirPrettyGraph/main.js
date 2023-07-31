// printPrettyGraph.cpp puts all elements as divs using HTML attributes as if they would be rendered in a pure HTML.
// However, we actually use cytoscape, which has its own rendering mechanisms.
// So we essentially use HTML as a fancy DSL to store our element data,
// and this script parses that fancy DSL using the DOM API and translates it into cytoscape's DSL.

// Get fallback and input
const sourcesNeeded = document.getElementById("sources-needed");
const input = document.getElementById("js-input");

// Create Cytoscape container and details
const container = document.createElement("main");
container.id = "cy";
const details = document.createElement("aside");
details.id = "details";
const addrDiv = document.createElement("div");
addrDiv.id = "addr";
const nameDiv = document.createElement("div");
nameDiv.id = "name";
const bodyDiv = document.createElement("div");
bodyDiv.id = "body";
details.appendChild(addrDiv);
details.appendChild(nameDiv);
details.appendChild(bodyDiv);
const options = document.createElement("aside");
options.id = "options";
function makeCheckmark(id, labelText) {
    const label = document.createElement("label");
    label.htmlFor = id;
    label.textContent = labelText
    const checkmark = document.createElement("input");
    checkmark.type = "checkbox";
    checkmark.id = id
    checkmark.checked = localStorage.getItem(id) === "true";
    checkmark.addEventListener("change", function () {
        localStorage.setItem(id, checkmark.checked.toString());
    });
    const container = document.createElement("div");
    container.appendChild(label);
    container.appendChild(checkmark);
    options.appendChild(container);
    return checkmark;
}
function makeTip(text) {
    const tip = document.createElement("p");
    tip.className = "tip";
    tip.textContent = text;
    options.appendChild(tip);
}
const childrenInsideParentsCheckmark = makeCheckmark("childrenInsideParents", "Children inside parents");
const lassoSelectionCheckmark = makeCheckmark("lassoSelection", "Lasso selection");
makeTip("Hold Z before drag to also drag nodes from incoming edges, hold X to also drag outgoing");

// Remove fallback and input, add container and associated
document.body.removeChild(sourcesNeeded);
// Don't actually remove so we can inspect the source, just hide
input.display = "none";
document.body.appendChild(container);
document.body.appendChild(options);
document.body.appendChild(details);

// Translate input into output.
// Specifically, read options, then parse nodes from `input` and return then in cytoscape format.
// See https://js.cytoscape.org/#getting-started/including-cytoscape.js and https://js.cytoscape.org/#cy.add
// for the cytoscape DSL for nodes and edges (we return an array of nodes and edges, like `elements`).
function translate() {
    // Options
    const childrenInsideParents = childrenInsideParentsCheckmark.checked;

    const elements = [];
    const elementsWithParents = new Map();
    for (const child of input.children) {
        elements.push({
            group: "nodes",
            data: {
                id: child.id,
                name: child.getElementsByClassName("name").item(0)?.innerHTML,
                body: child.getElementsByClassName("body").item(0)?.innerHTML,
            },
            classes: child.className.replaceAll("node-", ""),
        });
        for (const connected of child.getElementsByClassName("arrow")) {
            const target = connected.getAttribute("data-connected");
            const isChild = connected.hasAttribute("data-is-child");
            if (childrenInsideParents && isChild) {
                if (target in elementsWithParents) {
                    console.warn("Multiple parents for " + target + "!");
                }
                // A bit confusing: child is actually the parent here, and target is its child
                // `child` refers to input.children
                elementsWithParents.set(target, child.id);
            } else {
                elements.push({
                    group: "edges",
                    data: {
                        label: connected.innerHTML,
                        source: child.id,
                        target,
                        isChild
                    },
                    classes: connected.className.replaceAll("arrow-", ""),
                })
            }
        }
    }
    for (const [element, parent] of elementsWithParents.entries()) {
        const child = elements.find(e => e.data.id === element);
        if (child) {
            child.data.parent = parent;
        } else {
            console.error("Parent " + parent + " not found for " + element + "!");
        }
    }
    return elements;
}

// Create cytoscape graph
// See https://js.cytoscape.org/#getting-started/including-cytoscape.js and the rest of the page
// for an overview of the cytoscape DSL
const layout = {
    name: "fcose",
    nodeRepulsion: 100000,
    idealEdgeLength: edge => {
        if (edge.data("isChild")) {
            return 50;
        } else if (edge.target().hasClass("node-other")) {
            return 150;
        } else {
            return 450;
        }
    }
};
// noinspection JSUnresolvedReference
const graph = cytoscape({
    container,
    elements: translate(),
    style,
    layout,
});

// Recreate cytoscape graph without removing event listeners which get added after this script ends...
function regenerate() {
    const newNodes = translate();
    graph.batch(() => {
        graph.remove(graph.elements());
        graph.add(newNodes);
        graph.layout(layout).run();
    });
}