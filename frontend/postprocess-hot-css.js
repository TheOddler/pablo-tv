// This is a little post-processing script for elm-watch, that adds hot-reloading for CSS.
// It's polling the server, and supports `If-Modified-Since` as my server return 304 when there are no changes.
const hotReloadingCode = `
(function () {
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
      reloadCss(link, url);
    } catch (err) {
      console.error("Error while reloading CSS", err);
    }
  };

  function reloadCss(link, url) {
    // Creating a new element like this allows the browser to pre-load the css, preventing flicker on changes
    const replacement = link.cloneNode();
    replacement.href = url + '?_hot=' + Date.now();
    replacement.onload = () => {
      link.remove();
    };
    replacement.onerror = (e) => {
      replacement.remove();
    };
    link.parentNode.insertBefore(replacement, link.nextSibling);
  }

  async function tick() {
    const allCss = Array.from(document.querySelectorAll('link[rel="stylesheet"][href]'));
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
      return code;
    default:
      return patch(code);
  }
}
