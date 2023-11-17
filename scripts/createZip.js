const zl = require("zip-lib");

const OUTPUT = process.argv[2]

const zip = new zl.Zip()
zip.addFolder("assets", "assets")
zip.addFile("elm.js")
zip.addFile("index.html")
zip.archive(OUTPUT)

console.log(OUTPUT + " created")
console.log("    | elm.js")
console.log("    | index.html")
console.log("    | assets/*")
console.log("")