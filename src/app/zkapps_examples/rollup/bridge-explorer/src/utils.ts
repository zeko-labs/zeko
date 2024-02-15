function removeEmptyValues(obj: Record<string, string>) {
  return Object.fromEntries(Object.entries(obj).filter(([_, v]) => v != null && v !== ""));
}

export const generateQueryParams = (paramObj: Record<string, any>) => {
  const removedEmptyValuesObj = removeEmptyValues(paramObj);
  const queryParams = new URLSearchParams(removedEmptyValuesObj);
  return queryParams.toString();
};

export const generateUrlWithQueryParams = (URL: string, paramObj: Record<string, any>) => {
  const queryParams = generateQueryParams(paramObj);
  return URL + "?" + queryParams.toString();
};
