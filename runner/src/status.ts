export interface StatusWriter {
  append: (text: string) => void
  clear: () => void
}

export class NopStatusWriter implements StatusWriter {
  append(_: string): void {}
  clear(): void {}
}

export class TextAreaStatusWriter implements StatusWriter {
  textarea: HTMLTextAreaElement
  constructor(textarea: HTMLTextAreaElement) {
    this.textarea = textarea
  }

  append(text: string): void {
    this.textarea.value += text
  }

  clear(): void {
    this.textarea.value = ''
  }
}
