/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["**/*.eml.ml"],
  theme: {
    extend: {},
  },
  plugins: [
    require("@tailwindcss/forms"),
    require("@tailwindcss/typography"),
    require("@tailwindcss/aspect-ratio"),
  ],
}
