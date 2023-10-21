var observer = new MutationObserver(function(mutations) {
   var fullscreenOverlay = document.getElementById('bslib-full-screen-overlay');
   if (fullscreenOverlay) {
        $('#controls').draggable('disable');
    }else {
      $('#controls').draggable('enable');
  }
});

observer.observe(document, {attributes: false, childList: true, characterData: false, subtree:true});
