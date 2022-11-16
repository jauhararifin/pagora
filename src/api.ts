import { FunctionType, TypeKind } from './semantic'

export const apis: { [funcname: string]: FunctionType } = {
  draw_pixel: {
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
  get_width: {
    kind: TypeKind.FUNCTION,
    arguments: [],
    return: { kind: TypeKind.INTEGER }, // width
  },
  get_height: {
    kind: TypeKind.FUNCTION,
    arguments: [],
    return: { kind: TypeKind.INTEGER }, // height
  },
  output: {
    kind: TypeKind.FUNCTION,
    arguments: [{ kind: TypeKind.STRING }],
    return: { kind: TypeKind.VOID },
  },
  register_event: {
    kind: TypeKind.FUNCTION,
    arguments: [{ kind: TypeKind.STRING }],
    return: { kind: TypeKind.VOID },
  },
  // TODO: add function to registerEvent like `onMouseClick(functionName)`
  // TODO: add function to registerEvent like `onResize(functionName)`
  // TODO: add function to registerEvent like `onDraw(functionName)`
  // TODO: add function to registerEvent like `onTick(functionName)`
}
