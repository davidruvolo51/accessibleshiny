# Accessible Shiny

The accessible shiny package contains a series of accessible ui components for use in shiny applications. This project is in the early days of development and more fun things are on the way!

## Development

Use the `dev` branch for all development work, make all of the changes, and submit a new pull request. 

For developing this package, there are a number of npm packages required for this project. This allows us to use tools like autoprefixer, cssnano, and many more! In the future, there will be more need for these tools as we (or when we) move into javascript development.

Here's a list of tools that you will need.

- Command line tools: [Node.js and npm](https://nodejs.org) (running the nodeJS installer will also install npm)
- Application bundler: [Parcel](https://parceljs.org/getting_started.html)


Once nodejs and npm are installed, you can use npm to install parceljs.

```bash
npm install -g parcel-bundler
```

You can confirm the installation of these tools by running the following commands.

```bash
node -v
npm -v 
parcel --version
```

### Getting started with parcel and npm

At the moment, there Parcel is used to to build css files from scss files using `cssnano` and `autoprefixer`. These packages allow us to write styles in scss and leave the heavy lifting to parcel (adding vendor prefixing, compiling scss into css, minifying files, etc.).

To start working, you will need to install all of the dependencies. (You will only need to do this once.)

```bash
cd inst/assets/
npm run install
```

All of the scss files are located in the `src` folder. Open the scss file of your choice, make edits, and save. When all changes are ready, run the build scripts in `package.json` file.

```bash
npm run buildcss
npm run renamecss
```

`buildcss` will compile the scss files and the `renamecss` script will rename the css files to `*.min.css`. (For now this will be a 2-step process.)


### Build in the package

The package can be built in Rstudio or in the terminal. Here are the commands for running in the terminal.

```bash
cd path/to/parent/dir  # one level up from the package dir
R CMD build accessibleshiny
R CMD install accesibleshiny_[version].tar.gz
```