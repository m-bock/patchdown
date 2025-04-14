export const replaceEffectImpl = function (just) {
  return function (nothing) {
    return function (r) {
      return function (f) {
        return function (s) {
          return function () {
            return s.replace(r, function (match) {
              var groups = [];
              var group,
                i = 1;
              while (typeof (group = arguments[i++]) !== "number") {
                groups.push(group == null ? nothing : just(group));
              }
              return f(match)(groups)();
            });
          };
        };
      };
    };
  };
};

