// We need custom scripting to make node bodies visible when you hover over the nodes
let focusedNode = undefined;
let selectedNode = undefined;

const focus = function (node) {
    focusedNode = node;
    sidebar.style.display = "";
    addrDiv.textContent = node.data("id");
    const body = node.data("body");
    if (body) {
        bodyDiv.style.display = "";
        // We *want* to render HTML in the body
        bodyDiv.innerHTML = body;
    } else {
        bodyDiv.style.display = "none";
        bodyDiv.innerHTML = "";
    }
}

const defocus = function () {
    focusedNode = undefined;
    sidebar.style.display = "none";
    addrDiv.innerHTML = "";
    bodyDiv.innerHTML = "";
}

const highlight = function (node) {
    focus(node);
}

const unhighlight = function () {
    if (selectedNode) {
        focus(selectedNode);
    } else {
        defocus();
    }
}

const select = function (node) {
    selectedNode = node;
    focus(node);
}

const deselect = function () {
    if (focusedNode === selectedNode) {
        defocus();
    }
    selectedNode = undefined;
}

graph.on("mouseover", "node", function (event) {
    highlight(event.target);
});

graph.on("mouseout", "node", function (event) {
    unhighlight(event.target);
});

graph.on("tap", "node", function (event) {
    if (selectedNode === event.target) {
        deselect();
    } else {
        select(event.target);
    }
});