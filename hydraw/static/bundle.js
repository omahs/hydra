// Websocket to backend (proxied to hydra-node)

const protocol = window.location.protocol == "https:" ? "wss:" : "ws:";
const client = new WebSocket(protocol + "//" + window.location.host);

client.addEventListener("message", e => {
  const msg = JSON.parse(e.data);
  console.log("Received mesage", msg);
});

// Canvas

const CANVAS_SIZE = 32;

const canvas = document.querySelector('canvas');
const ctx = canvas.getContext('2d');
const canvasScale = {
  x: canvas.width / CANVAS_SIZE,
  y: canvas.height / CANVAS_SIZE,
}
ctx.scale(canvasScale.x, canvasScale.y);
console.log("canvasScale", canvasScale);

const drawPixel = (x, y, rgb) => {
  const [r, g, b] = rgb;
  ctx.fillStyle = `rgb(${r}, ${g}, ${b})`;
  ctx.fillRect(x, y, 1, 1);
}


canvas.addEventListener('click', function (e) {
  console.log("event", e);
  const canvasPosition = {
    x: canvas.offsetLeft,
    y: canvas.offsetTop
  };
  console.log("canvasPosition", canvasPosition);
  const clickedPixel = {
    x: (e.pageX - canvasPosition.x) / canvasScale.x,
    y: (e.pageY - canvasPosition.y) / canvasScale.y
  };
  console.log("clickedPixel", clickedPixel);

  const x = Math.floor(clickedPixel.x);
  const y = Math.floor(clickedPixel.y);

  const [r, g, b] = currentColor;
  fetch(`/paint/${x}/${y}/${r}/${g}/${b}`)
    .then(() => console.log("Ok"))
    .catch(e => console.log("Error", e));
});

// Color picker

let currentColor = [255, 0, 0];
const currentColorElement = document.querySelector('#current-color');
const picker = new Picker(currentColorElement);

currentColorElement.style.background = `rgb(${currentColor[0]}, ${currentColor[1]}, ${currentColor[2]})`;

picker.onDone = function (color) {
  console.log("Color picked:", color);
  currentColor = color.rgba;
  currentColorElement.style.background = color.rgbaString;
};
