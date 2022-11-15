import { FunctionType, TypeKind } from './semantic'

export const apis: { [funcname: string]: FunctionType } = {
  drawPixel: {
    kind: TypeKind.FUNCTION,
    arguments: [
      { kind: TypeKind.INTEGER }, // x
      { kind: TypeKind.INTEGER }, // y
      { kind: TypeKind.INTEGER }, // r
      { kind: TypeKind.INTEGER }, // g
      { kind: TypeKind.INTEGER }, // b
    ],
    return: { kind: TypeKind.VOID },
  },
  getWidth: {
    kind: TypeKind.FUNCTION,
    arguments: [],
    return: { kind: TypeKind.INTEGER }, // width
  },
  getHeight: {
    kind: TypeKind.FUNCTION,
    arguments: [],
    return: { kind: TypeKind.INTEGER }, // height
  },
  output: {
    kind: TypeKind.FUNCTION,
    arguments: [{ kind: TypeKind.STRING }],
    return: { kind: TypeKind.VOID },
  },
  // TODO: add function to registerEvent like `onMouseClick(functionName)`
  // TODO: add function to registerEvent like `onResize(functionName)`
  // TODO: add function to registerEvent like `onDraw(functionName)`
  // TODO: add function to registerEvent like `onTick(functionName)`
}
