// Action transform, remove, rename column.
var accion = function(id, col, acc, val) {
  Shiny.setInputValue(id, [col, acc, val], {priority: "event"});
};