import * as jsyaml from "js-yaml";

export const yamlToJson = (yaml) => () => {
  return jsyaml.load(yaml);
};

export const printYaml = (json) => {
  return jsyaml.dump(json);
};
