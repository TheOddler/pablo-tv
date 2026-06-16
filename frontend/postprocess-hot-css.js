// This is a little post-processing script for elm-watch, that adds hot-reloading for CSS.
// It's polling the server, and supports `If-Modified-Since` as my server return 304 when there are no changes.
const hotReloadingCode = `
(function () {
  // Note that since all my css builds to main.css, I only consider that file for hot-reloading, which makes the script much simpler
  const url = "/main.css";
  let last = null;

  async function check() {
    try {
      const res = await fetch(
        url,
        {
          method: 'HEAD',
          headers: last ? { 'If-Modified-Since': last } : {},
          cache: 'no-store'
        }
      );
      if (res.status === 304) return; // unchanged
      if (!res.ok) return; // error
      // Remember the last-modified date the server sends
      last = res.headers.get('Last-Modified');
      reloadCss();
    } catch (err) {
      console.error("Error while reloading CSS", err);
    }
  };

  function reloadCss() {
    // Creating a new element like this allows the browser to pre-load the css, preventing flicker on changes.
    // It's also possible that if the JS reloaded in the middle of this, we ended up with a new element without having removed the old one. So always remove ALL old ones.
    let allExisting = Array.from(document.querySelectorAll('link[rel="stylesheet"][href]')).filter(l => l.href.split('?')[0].endsWith(url));

    const newOne = document.createElement('link');
    newOne.rel = 'stylesheet';
    newOne.href = url + '?_hot=' + Date.now();

    newOne.onload = () => {
      allExisting.forEach(l => l.remove());
    };
    newOne.onerror = (e) => {
      newOne.remove();
    };
    document.head.appendChild(newOne);
  }

  async function tick() {
    await check();
    setTimeout(tick, 1000);
  }
  tick();
})();
`;

function patch(code) {
  return hotReloadingCode
    + '\n'
    + code;
}

export default function postprocess({ code, compilationMode }) {
  switch (compilationMode) {
    case "optimize":
      return code;
    default:
      return patch(code);
  }
}
