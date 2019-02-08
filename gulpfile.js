const fs = require("fs")
const gulp = require("gulp")
const {series, parallel} = gulp
const child_process = require("child_process")
const gexec = require("gulp-exec")
const nunjucksGulp = require("gulp-nunjucks")
const nunjucks = require("nunjucks")
const htmlmin = require("gulp-htmlmin")
const cleanCSS = require("gulp-clean-css")
const imagemin = require("gulp-imagemin")
const workbox = require("workbox-build")

function renderTemplate(path) {
    nunjucks.render(path, {}, (err, res) => {
        if (err != null) {
            console.error(err)
            return
        }
        fs.writeFileSync("dist/" + path, res);
    })
}

let env = new nunjucks.Environment(new nunjucks.FileSystemLoader("templates"), { autoescape: false });

function html () {
    return gulp.src([
        "templates/index.html",
        "templates/about.html",
        "templates/404.html"
    ])
        .pipe(nunjucksGulp.compile(
            { wrapperClass: ""},
            { env }
        ))
        // .pipe(htmlmin({ collapseWhitespace: true }))
        .pipe(gulp.dest("dist"))
}

function literateHaskell() {
    child_process.execSync("cabal new-build")
    return gulp.src("templates/posts/*.lhs")
        .pipe(gexec('pandoc <%= file.path %> -o <%= file.path %>.html'))
        .pipe(gexec.reporter())
}

function posts() {
    return gulp.src([
        "templates/posts/free-monads.html",
        "templates/posts/peano-algebras-in-haskell.html",
        "templates/posts/monadicity.html",
        "templates/posts/finite-state-machines.html",
        "templates/posts/kleisli-categories-and-free-monads.html",
        "templates/posts/categories-with-monadic-effects.html",
        "templates/posts/monadic-io.html",
    ])
        .pipe(nunjucksGulp.compile(
            { wrapperClass: "post" },
            { env }
        ))
        // .pipe(htmlmin({ collapseWhitespace: true }))
        .pipe(gulp.dest("dist/posts"))
}

function css() {
    return gulp.src("assets/*.css")
        .pipe(cleanCSS())
        .pipe(gulp.dest("dist/assets"))
}

function fonts() {
    return gulp.src("assets/*.ttf")
        .pipe(gulp.dest("dist/assets"));
}

function pdflatex() {
    return gulp.src("latex/*.tex")
        .pipe(gexec("pdflatex -interaction=nonstopmode -shell-escape <%= file.path %> -output-directory=./latex", {cwd: "latex"}))
        .pipe(gexec.reporter());
}

function copyPdflatexSVGs() {
    return gulp.src("latex/*.svg")
        .pipe(gulp.dest("images"));
}

function images() {
    return gulp.src("images/*.{png,jpg,svg}")
        .pipe(imagemin())
        .pipe(gulp.dest("dist/images"));
}

function copyManifest() {
    return gulp.src("./manifest.json")
        .pipe(gulp.dest("dist"))
}

function js() {
    return gulp.src([
            "assets/*.js",
            "node_modules/workbox-sw/build/workbox-sw.js*",
            "node_modules/workbox-routing/build/workbox-routing.prod.js*",
            "node_modules/workbox-precaching/build/workbox-precaching.prod.js*",
            "bower_components/html5shiv/dist/html5shiv*.js"
        ])
        .pipe(gulp.dest("dist/assets"))
}

function injectManifest() {
    return workbox.injectManifest({
        globDirectory: "./dist",
        globPatterns: [
            "assets/*.{css,js}",
            "images/*.{svg,jpg,png}",
            "bower_components/html5shiv/dist/html5shiv.js",
            "bower_components/html5shiv/dist/html5shiv-printshiv.js",
        ],
        swSrc: "sw.js",
        swDest: "dist/sw.js"
    })
}

exports.posts    = series(literateHaskell, posts)
exports.html     = parallel(html, css, fonts, exports.posts)
exports.pdflatex = series(pdflatex, copyPdflatexSVGs, images)
exports.full     = series(parallel(html, series(exports.pdflatex, images), css, fonts, js, copyManifest, exports.posts), injectManifest)
exports.default  = series(parallel(html, images, css, fonts, js, copyManifest, exports.posts), injectManifest)
