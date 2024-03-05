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
    startTime : (U8, U8),
    endTime : (U8, U8)
}

# Run this app with some arguments: `simple --startTime 12 00 --endTime 14 00`
main : Task {} I32
main =
    rawArgs <- Arg.list |> Task.await

    args : Result Args _
    args = Crap.parseArgs rawArgs

    when args is
        Ok { startTime, endTime } ->
            Stdout.line "We start at $(Num.toStr startTime.0):$(Num.toStr startTime.1) and end at $(Num.toStr endTime.0):$(Num.toStr endTime.1)"
        Err _ ->
            {} <- Stderr.line "Could not parse arguments :(" |> Task.await
            Task.err -1
