export function formatUnits(value: bigint | undefined, decimals: number) {
  if (value === undefined) {
    return undefined;
  }
  let display = value.toString();

  const negative = display.startsWith("-");
  if (negative) display = display.slice(1);

  display = display.padStart(decimals, "0");

  let [integer, fraction] = [
    display.slice(0, display.length - decimals),
    display.slice(display.length - decimals),
  ];
  fraction = fraction.replace(/(0+)$/, "");
  return `${negative ? "-" : ""}${integer || "0"}${
    fraction ? `.${fraction}` : ""
  }`;
}

export const formatMina = (value: bigint | undefined) => {
  return formatUnits(value, 9);
};

export const parseUnits = (value: `${number}`, decimals: number) => {
  let [integer, fraction = "0"] = value.split(".");

  const negative = integer.startsWith("-");
  if (negative) integer = integer.slice(1);

  // trim leading zeros.
  fraction = fraction.replace(/(0+)$/, "");

  // round off if the fraction is larger than the number of decimals.
  if (decimals === 0) {
    integer = `${Math.round(Number(`${integer}.${fraction}`))}`;
    fraction = "";
  } else if (fraction.length > decimals) {
    const [left, unit, right] = [
      fraction.slice(0, decimals - 1),
      fraction.slice(decimals - 1, decimals),
      fraction.slice(decimals),
    ];

    const zeroes = left.match(/^0+/)?.[0].length ?? 0;

    const rounded = Math.round(Number(`${unit}.${right}`));
    if (rounded > 9)
      fraction = `${zeroes ? left.slice(0, zeroes - 1) : ""}${
        BigInt(left) + 1n
      }0`;
    else fraction = `${left}${rounded}`;

    if (fraction.length > decimals) {
      fraction = fraction.slice(1);
      integer = `${BigInt(integer) + 1n}`;
    }

    fraction = fraction.slice(0, decimals);
  } else {
    fraction = fraction.padEnd(decimals, "0");
  }

  return BigInt(`${negative ? "-" : ""}${integer}${fraction}`);
};

export const parseMina = (value: `${number}`) => {
  return parseUnits(value, 9);
};

export const formatAddress = (
  address: string | undefined
): string | undefined => {
  if (address === undefined) return undefined;
  return `${address.substring(0, 10)}...${address.substring(
    address.length - 10
  )}`;
};
