const showMessage = function(show, delay = 0) {
  const message = document.getElementById("status");
  if (message) {
    message.style.transition = "opacity " + delay + "s";
    message.style.opacity = show ? 1 : 0;
  }
};

document.addEventListener("DOMContentLoaded", function() {
  showMessage(false, 0);
});

document.addEventListener("htmx:load", function() {
  showMessage(true, 0);
  setTimeout(function() {
    showMessage(false, 1);
  }, 1500);
});
