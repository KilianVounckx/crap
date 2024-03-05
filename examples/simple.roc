app ""
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.8.1/x8URkvfyi9I0QhmVG98roKBUs_AZRkLFwFJVJ3942YA.tar.br",
        crap: "../package/main.roc",
    }
    imports [
        cli.Arg,
        cli.Stderr,
        cli.Stdout,
        cli.Task.{ Task },

        crap.Crap,
    ]
    provides [main] to cli

Args : {
    name : Str,
    age : U8,
}

# Run this app with some arguments: `simple --name Frodo --age 50`
main : Task {} I32
main =
    rawArgs <- Arg.list |> Task.await

    args : Result Args _
    args = Crap.parseArgs rawArgs

    when args is
        Ok {name, age} ->
            Stdout.line "$(name) is $(Num.toStr age) years old."
        Err _ ->
            {} <- Stderr.line "Could not parse arguments :(" |> Task.await
            Task.err -1
