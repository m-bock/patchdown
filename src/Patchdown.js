export const replaceEffectImpl = (just, nothing, r, f, s) => {
  s.replace(r, (match) => {
    const groups = [];
    let group,
      i = 1;
    while (typeof (group = arguments[i++]) !== "number") {
      groups.push(group == null ? nothing : just(group));
    }

    return f(match, groups);
  });
};
