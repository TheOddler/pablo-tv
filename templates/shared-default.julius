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

      // Only consider elements that are in the direction of the arrow key
      const a = angleDiff(angle(curPos, point), wantedAngle);
      if (a > Math.PI / 4) return;

      // Ignore elements too far off screen (a little off-screen we allow)
      const allowedOffScreen = 0.3;
      if (point.x < -window.innerWidth * allowedOffScreen
        || point.x > window.innerWidth * (1 + allowedOffScreen)
        || point.y < -window.innerHeight * allowedOffScreen
        || point.y > window.innerHeight * (1 + allowedOffScreen)
      ) return;

      // Remember closest element
      const dist = distance(curPos, point) / Math.cos(a * 1.2); // Scale by the angle so that we prefer elements in the direction
      if (dist < minDistance) {
        minDistance = dist;
        targetElement = element;
      }
    });

    if (targetElement) {
      targetElement.focus();
      targetElement.scrollIntoView({
        behavior: "smooth",
        block: "center",
        inline: "center",
      });
    }
  };

  let lastEventTime = 0; // To slow down repeat rate, but still allow key spamming
  document.addEventListener('keydown', (event) => {
    switch (event.key) {
      case 'ArrowUp':
      case 'ArrowDown':
      case 'ArrowLeft':
      case 'ArrowRight':
        // Limit the repeat rate
        if (Date.now() - lastEventTime > 100) {
          moveFocus(event.key);
          lastEventTime = Date.now();
        }
        // Prevent default (that is scrolling),
        // as we scroll to the selected element
        event.preventDefault();
        break;
      // Allow going back with the backspace key for better keyboard navigation
      case 'Backspace':
        history.back();
        event.preventDefault();
        break;
    }
  });
  document.addEventListener('keyup', (event) => {
    switch (event.key) {
      case 'ArrowUp':
      case 'ArrowDown':
      case 'ArrowLeft':
      case 'ArrowRight':
        // Allow spamming by resetting the time
        lastEventTime = 0;
        event.preventDefault();
        break;
    }
  });
})();
