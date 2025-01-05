(() => {
  /**
   * Code to make arrow keys navigate between focusable elements.
   */

  /**
   * @param {Element} element
  */
  const getMiddle = (element) => {
    const rect = element.getBoundingClientRect();
    return {
      x: rect.left + rect.width / 2,
      y: rect.top + rect.height / 2,
    };
  };

  /**
   * @typedef {Object} Point
   * @property {number} x
   * @property {number} y
   */

  /** @typedef {'ArrowUp' | 'ArrowDown' | 'ArrowLeft' | 'ArrowRight'} Direction */

  /**
   * @param {Point} a
   * @param {Point} b
  */
  const angle = (a, b) => Math.atan2(b.y - a.y, b.x - a.x);

  /**
   * @param {Point} a
   * @param {Point} b
  */
  const distance = (a, b) => Math.hypot(b.x - a.x, b.y - a.y);

  /**
   * @param {number} a
   * @param {number} b
  */
  const angleDiff = (a, b) => {
    const diff = b - a;
    const angle = Math.atan2(Math.sin(diff), Math.cos(diff));
    return Math.abs(angle);
  };

  /**
   * @param {Direction} direction
  */
  const dirToAngle = (direction) => {
    switch (direction) {
      case 'ArrowUp':
        return Math.PI * 3 / 2;
      case 'ArrowDown':
        return Math.PI / 2;
      case 'ArrowLeft':
        return Math.PI;
      case 'ArrowRight':
        return 0;
    }
  };

  const getFocusableElements = () => {
    return Array.from(document.querySelectorAll(
      'a[href], area[href], input:not([disabled]), select:not([disabled]), textarea:not([disabled]), button:not([disabled]), iframe, object, embed, [tabindex], [contenteditable], audio[controls], video[controls], summary'
    )).filter(el => !el.hasAttribute('tabindex') || el.tabIndex >= 0);
  };

  /**
   * @param {Direction} direction
  */
  const moveFocus = (direction) => {
    const currentElement = document.activeElement;
    const focusableElements = getFocusableElements();
    if (focusableElements.length === 0) return;
    if (!currentElement) {
      focusableElements[0].focus();
      return;
    }

    const curPos = getMiddle(currentElement);
    const wantedAngle = dirToAngle(direction);

    /** @type {Element | null} */
    let targetElement = null;
    let minDistance = Infinity;

    focusableElements.forEach((element) => {
      if (element === currentElement) return;

      const point = getMiddle(element);
      if (angleDiff(angle(curPos, point), wantedAngle) > Math.PI / 4) return;

      const dist = distance(curPos, point);
      if (dist < minDistance) {
        minDistance = dist;
        targetElement = element;
      }
    });

    if (targetElement) {
      targetElement.focus();
      targetElement.scrollIntoView({
        behavior: "smooth",
        // block is vertical scrolling, center it when moving vertically.
        // Otherwise scroll as little as possible (possibly not at all)
        block:
          direction == 'ArrowUp' || direction == 'ArrowDown' ?
            "center" :
            "nearest",
        // inline is horizontal scrolling, handled similarly as vertical
        inline:
          direction == 'ArrowLeft' || direction == 'ArrowRight' ?
            "center" :
            "nearest",
      });
    }
  };

  document.addEventListener('keydown', (event) => {
    switch (event.key) {
      case 'ArrowUp':
      case 'ArrowDown':
      case 'ArrowLeft':
      case 'ArrowRight':
        moveFocus(event.key);
        // Prevent default (that is scrolling),
        // as we scroll to the selected element
        event.preventDefault();
        break;
    }
  });
})();