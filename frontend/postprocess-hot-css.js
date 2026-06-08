// This is a little post-processing script for elm-watch, that adds hot-reloading for CSS.
// It's polling the server, and supports `If-Modified-Since` as my server return 304 when there are no changes.
const hotReloadingCode = `
(function () {
  // This doesn't find dynamically added css, but we don't have that so that's OK.
  const allCss = Array.from(document.querySelectorAll('link[rel="stylesheet"][href]'));
  const last = new Map(); // url -> last-modified value
  async function check(link) {
    try {
      const url = link.href.split('?')[0];
      const res = await fetch(
        url,
        {
          method: 'HEAD',
          headers: last.has(url) ? { 'If-Modified-Since': last.get(url) } : {},
          cache: 'no-store'
        }
      );
      if (res.status === 304) return; // unchanged
      if (!res.ok) return; // error
      // Remember the last-modified date the server sends
      last.set(url, res.headers.get('Last-Modified'));
      link.href = url + '?_hot=' + Date.now();
    } catch (err) {
      console.error("Error while reloading CSS", err);
    }
  };

  async function tick() {
    await Promise.all(allCss.map(check));
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
      return minify(patch(code));
    default:
      return patch(code);
  }
}
