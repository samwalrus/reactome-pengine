pengines = require('pengines');

peng = pengines({
    server: "https://apps.nms.kcl.ac.uk/reactome-pengine",
    sourceText: 'small_pathway(P):- ridPathway_links(P,L), length(L,S), S<35.',
    ask: "small_pathway(X)",
    chunk: 100,
}
).on('success', handleSuccess).on('error', handleError);
function handleSuccess(result) {
    console.log(result)
}
function handleError(result) {
    console.error(result)
}
