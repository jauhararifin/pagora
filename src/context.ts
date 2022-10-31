export interface Context {
    errors: Error[],
    sourceCode: string,
}

export interface Error {
}

export function createContext(sourceCode: string): Context {
    return {
        sourceCode: sourceCode,
        errors: [],
    }
}