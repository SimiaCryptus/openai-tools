function imageHandler(imgUrl) {
  function(e) {
    e.preventDefault();
    var imageDiv = document.createElement('div');
    imageDiv.style.position = 'fixed';
    imageDiv.style.top = '0';
    imageDiv.style.left = '0';
    imageDiv.style.width = '100%';
    imageDiv.style.height = '100%';
    imageDiv.style.backgroundColor = 'rgba(0, 0, 0, 0.5)';
    imageDiv.style.zIndex = '9999';
    imageDiv.style.textAlign = 'center';
    var image = document.createElement('img');
    image.src = '/image.png';
    image.style.maxWidth = '100%';
    image.style.maxHeight = '100%';
    image.style.marginTop = '100px';
    imageDiv.appendChild(image);
    document.body.appendChild(imageDiv);
    imageDiv.onclick = function() {
      document.body.removeChild(imageDiv);
    };
    document.onkeydown = function(e) {
      if (e.keyCode === 27) {
        document.body.removeChild(imageDiv);
      }
    };
  };
}



var links = document.getElementsByTagName('a');
for (var i = 0; i < links.length; i++) {
  if (links[i].href.match(/\.png$/)) {
    links[i].onclick = imageHandler(links[i].href)
  }
}

