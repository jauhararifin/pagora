import { Value } from './value'

export class SymbolTable {
  scopes: Array<Set<string>> = []
  names: Map<string, Value[]> = new Map()

  reset(): void {
    this.scopes = []
    this.names = new Map()
  }

  addScope(): void {
    this.scopes.push(new Set())
  }

  setSymbol(name: string, value: Value): void {
    const currentScope = this.scopes[this.scopes.length - 1]

    if (currentScope.has(name)) {
      const vars = this.names.get(name)
      if (vars === undefined) {
        throw new Error(
          `invalid state, current scope has symbol "${name}" but names don't have it`
        )
      }
      vars[vars.length - 1] = value
    } else {
      if (!this.names.has(name)) {
        this.names.set(name, [])
      }
      this.names.get(name)!.push(value)
      currentScope.add(name)
    }
  }

  getSymbol(name: string): Value {
    const variable = this.names.get(name)
    if (variable === undefined) {
      throw new Error(`invalid state, searching undefined symbol "${name}"`)
    }
    return variable[variable.length - 1]
  }

  popScope(): void {
    const poppedSymbols = this.scopes.pop()
    if (poppedSymbols === undefined) {
      return
    }

    for (const sym of poppedSymbols) {
      const values = this.names.get(sym)
      if (values === undefined) {
        throw new Error(
          `invalid state, current poppedSymbols has symbol "${sym}" but names don't have it`
        )
      }

      values.pop()
      if (values.length === 0) {
        this.names.delete(sym)
      }
    }
  }
}
