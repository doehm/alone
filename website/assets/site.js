// Hover / focus tooltips for chart marks carrying data-tip="Value|line 2|line 3".
// Tooltips only enhance: every value is also visible as a label or in a table.
(() => {
  const tip = document.createElement("div");
  tip.className = "tip";
  tip.setAttribute("role", "tooltip");
  document.body.appendChild(tip);

  const fill = (el) => {
    tip.replaceChildren();
    el.dataset.tip.split("|").forEach((line, i) => {
      const d = document.createElement("div");
      d.textContent = line;
      if (i === 0) d.className = "tip-strong";
      tip.appendChild(d);
    });
  };

  const place = (x, y) => {
    const r = tip.getBoundingClientRect();
    let left = x + 14, top = y - r.height - 12;
    if (left + r.width > window.innerWidth - 8) left = x - r.width - 14;
    if (top < 8) top = y + 18;
    tip.style.left = Math.max(8, left) + "px";
    tip.style.top = top + "px";
  };

  const hide = () => { tip.style.opacity = 0; };

  document.addEventListener("pointerover", (e) => {
    const el = e.target.closest("[data-tip]");
    if (!el) return;
    fill(el);
    tip.style.opacity = 1;
    place(e.clientX, e.clientY);
  });
  document.addEventListener("pointermove", (e) => {
    if (e.target.closest("[data-tip]")) place(e.clientX, e.clientY);
  });
  document.addEventListener("pointerout", (e) => {
    if (e.target.closest("[data-tip]")) hide();
  });
  document.addEventListener("focusin", (e) => {
    const el = e.target.closest("[data-tip]");
    if (!el) return;
    fill(el);
    tip.style.opacity = 1;
    const b = el.getBoundingClientRect();
    place(b.left + b.width / 2, b.top);
  });
  document.addEventListener("focusout", hide);
  window.addEventListener("scroll", hide, { passive: true });
})();
