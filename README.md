# OwO Format (Colors)

This crate provides a procedural macro, `format_args_colored!`, which transforms
a small DSL to a composition of `format_args!` and trait method calls.

```rs
logln!(
    :bold .{
        :green "This executable was started with arguments:",
        :underline :color(CssColors::Orange)
            std::env::args().collect::<Vec<_>>().join(" ")
    };
    :italic :dimmed :yellow "Note that the first argument is the executable path."
);
```

This was originally a declarative macro in under 40 SLOC. That [original can be
found here].

If you want to use this macro, it isn't ready. It works, but you probably
shouldn't add this crate as a dependency.

The original is small and useful however; just copy the declarative macro into
your codebase and use it until you find its limitations.

For now, I'm satisfied with this experiment. It has exactly one more useful
feature than the declarative version: you are able to pass generics and
arguments to extension trait methods within the understood syntax of the DSL.
This was not possible with the `macro_rules!` version, or at least, it would be unwieldy.

Until [owo-colors] fixes issue [#45] (perhaps this macro can help), this project is
dead in the water. The original macro has the same problem, I thought it was
an implementation problem on my end, which why there are two. Neither work.

[owo-colors]: https://github.com/jam1garner/owo-colors
[#45]: https://github.com/jam1garner/owo-colors/issues/45
[original can be found here]: https://github.com/spikespaz/allfollow/blob/372b54b830964f6530f108e76bc2e0abee336bb6/src/fmt_colors.rs
