import { readFile } from 'fs'
import { compile } from '../compiler'

const file = process.argv[2]
readFile(file, (err, data) => {
  if (err != null) {
    console.log(err)
    return
  }

  const sourceCode = data.toString()
  console.log(sourceCode)
  const program = compile(sourceCode)
  console.log(program)
})
