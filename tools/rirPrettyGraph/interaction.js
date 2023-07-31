// Add option event handlers which affect the graph

childrenInsideParentsCheckmark.addEventListener("change", function () {
    regenerate();
});
lassoSelectionCheckmark.addEventListener("change", function () {
    // noinspection JSUnresolvedReference
    graph.lassoSelectionEnabled(lassoSelectionCheckmark.checked);
});

// Enable autopan-on-drag
// noinspection JSUnresolvedReference
graph.autopanOnDrag({
    enabled: true,
    selector: "node",
    speed: 10,
});

// We need custom scripting to make node bodies visible when you hover over or click on the nodes,
// and also click on edges to show their label, source, and target.
let focused = undefined;
let selected = undefined;

const focus = function (elem) {
    focused = elem;
    if (elem.isNode()) {
        details.style.display = "";
        addrDiv.textContent = elem.data("id");
        const name = elem.data("name");
        if (name) {
            nameDiv.style.display = "";
            // We *want* to render HTML because the name is already escaped
            nameDiv.innerHTML = name;
        } else {
            nameDiv.style.display = "none";
            nameDiv.innerHTML = "";
        }
        const body = elem.data("body");
        if (body) {
            bodyDiv.style.display = "";
            // We *want* to render HTML in the body
            bodyDiv.innerHTML = body;
        } else {
            bodyDiv.style.display = "none";
            bodyDiv.innerHTML = "";
        }
    } else if (elem.isEdge()) {
        const source = elem.source();
        const target = elem.target();
        details.style.display = "";
        addrDiv.textContent = elem.data("label");
        nameDiv.style.display = "";
        nameDiv.textContent = elem.classes().filter(c => c !== "arrow").join(" ");
        bodyDiv.style.display = "";
        const render = function (node) {
            let text = node.id();
            if (node.data("name")) {
                text += ` = ${node.data("name")}`;
            }
            return escapeHtml(text);
        }
        bodyDiv.innerHTML = `
    <p>Source: ${render(source)}</p>
    <p>Target: ${render(target)}</p>
    `;
    } else {
        throw new Error(`Bad element: ${elem}`);
    }
}

const defocus = function () {
    focused = undefined;
    details.style.display = "none";
    addrDiv.innerHTML = "";
    nameDiv.innerHTML = "";
    bodyDiv.innerHTML = "";
}

const highlight = function (elem) {
    focus(elem);
}

const unhighlight = function () {
    if (selected) {
        focus(selected);
    } else {
        defocus();
    }
}

const select = function (elem) {
    selected = elem;
    focus(elem);
}

const deselect = function () {
    if (focused === selected) {
        defocus();
    }
    selected = undefined;
}

graph.on("mouseover", "node", function (event) {
    highlight(event.target);
});

graph.on("mouseout", "node", function (event) {
    unhighlight(event.target);
});

graph.on("tap", function (event) {
    if (!event.target || !event.target.isNode || selected === event.target) {
        deselect();
    } else {
        select(event.target);
    }
});

// Make it so Z-grab also grabs incoming edges, and X-grab also grabs outgoing edges
let isZPressed = false;
let isXPressed = false;
document.addEventListener("keydown", function (event) {
    switch (event.key) {
        case "z":
            isZPressed = true;
            break;
        case "x":
            isXPressed = true;
            break;
        default:
            break;
    }
});
document.addEventListener("keyup", function (event) {
    switch (event.key) {
        case "z":
            isZPressed = false;
            break;
        case "x":
            isXPressed = false;
            break;
        default:
            break;
    }
});
let dragAlongPosition;
let dragAlongNodes;
graph.on("grab", "node", function(event) {
    const dragAlongSource = event.target;
    const p = dragAlongSource.position();
    dragAlongPosition = { x: p.x, y: p.y };
    dragAlongNodes = graph.collection();
    if (isZPressed) {
        dragAlongNodes.merge(dragAlongSource.incomers());
    }
    if (isXPressed) {
        dragAlongNodes.merge(dragAlongSource.outgoers());
    }
    dragAlongNodes.unmerge(dragAlongSource);
});
graph.on("drag", "node", function (event) {
    const p = event.target.position();
    const dragAlongDelta = { x: p.x - dragAlongPosition.x, y: p.y - dragAlongPosition.y };
    dragAlongPosition = { x: p.x, y: p.y };
    dragAlongNodes.shift(dragAlongDelta);
});