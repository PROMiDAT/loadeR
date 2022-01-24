Shiny.addCustomMessageHandler("updateLabel",
  function(message) {
    for (var i = 0; i < message.ids.length; i++) {
      element = $("[data-id=" + message.ids[i] + "]");
      for (var j = 0; j < element.length; j++) {
        element[j].innerHTML = message.values[i];
      }
    }
  }
);