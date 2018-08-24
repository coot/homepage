const fs = require("fs")
const gulp = require("gulp")
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

gulp.task("html", () => {
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
})

gulp.task("html:posts:lhs", () => {
    return gulp.src("templates/posts/*.lhs")
        .pipe(gexec('ghc -Wall <%= file.path %>'))
        .pipe(gexec('pandoc <%= file.path %> -o <%= file.path %>.html'))
        .pipe(gexec.reporter())
})

gulp.task("html:posts", ["html:posts:lhs"], () => {
    gulp.src([
        "templates/posts/free-monads.html",
        "templates/posts/peano-algebras-in-haskell.html",
        "templates/posts/monadicity.html",
        "templates/posts/finite-state-machines.html"
    ])
        .pipe(nunjucksGulp.compile(
            { wrapperClass: "post" },
            { env }
        ))
        // .pipe(htmlmin({ collapseWhitespace: true }))
        .pipe(gulp.dest("dist/posts"))
})

gulp.task("css", () => {
    return gulp.src("assets/*.css")
        .pipe(cleanCSS())
        .pipe(gulp.dest("dist/assets"))
})

function execPromise(cmd, obj, cb) {
    return new Promise((resolve, reject) => {
        child_process.exec(cmd, obj, (error, stdout, stderr) => {
            if (error) {
                reject({stdout, stderr})
            } else {
                resolve({stdout, stderr})
            }
        })
    })
}

gulp.task("images:latex", (cb) => {
    let opts = {cwd: "latex"}
    let file = "free-cat-morphism.tex"
    let cmd = "pdflatex -interaction=nonstopmode -shell-escape "
    let log = ({stdout, stderr}) => {
        console.log(stdout)
        console.log(stderr)
    }
    execPromise(cmd + file, opts)
        .then((args) => {
            log(args)
            cb()
        })
        .catch((args) => {
            log(args)
            cb()
            throw Error("free-cat-morphism.tex failed")
        })
    gulp.src("latex/*.svg")
        .pipe(gulp.dest("images"))
})

gulp.task("images", () => {
    return gulp.src("images/*.{png,jpg,svg}")
        .pipe(imagemin())
        .pipe(gulp.dest("dist/images"))
})

gulp.task("js", () => {
    return gulp.src([
            "assets/*.js",
            "node_modules/workbox-sw/build/workbox-sw.js*",
            "node_modules/workbox-routing/build/workbox-routing.prod.js*",
            "node_modules/workbox-precaching/build/workbox-precaching.prod.js*",
            "bower_components/html5shiv/dist/html5shiv*.js"
        ])
        .pipe(gulp.dest("dist/assets"))
})

gulp.task("default", ["html", "html:posts", "images", "css", "js"], () => {
    return workbox.injectManifest({
        globDirectory: "./dist",
        globPatterns: [
            "images/*.{svg,jpg,png}",
            "bower_components/html5shiv/dist/html5shiv.js",
            "bower_components/html5shiv/dist/html5shiv-printshiv.js",
        ],
        swSrc: "./dev/sw.js",
        swDest: "./dist/sw.js"
    })
})
