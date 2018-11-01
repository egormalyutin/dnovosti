// Generated by CoffeeScript 2.3.1
(function() {
  var $, $$, addImage, addText, postContent, postForm, postName;
  $ = function(...args) {
    return document.querySelector(...args);
  };
  $$ = function(...args) {
    return document.querySelectorAll(...args);
  };
  postForm = $("#post-form");
  postContent = $('#post-content');
  addText = $('#add-text');
  addImage = $('#add-image');
  postName = $("#post-name");
  addText.onclick = function() {
    var elem;
    elem = document.createElement("input");
    elem.type = "text";
    return postContent.appendChild(elem);
  };
  addImage.onclick = function() {
    var elem;
    elem = document.createElement("input");
    elem.type = "file";
    return postContent.appendChild(elem);
  };
  return postForm.onsubmit = function() {
    var child, i, len, n, ref;
    n = 0;
    ref = [...postContent.children];
    for (i = 0, len = ref.length; i < len; i++) {
      child = ref[i];
      if (child.type === "text") {
        child.name = "text" + n;
      } else if (child.type === "file") {
        child.name = "image" + n;
      }
      n++;
    }
    postName.name = "name";
    return true;
  };
})();
