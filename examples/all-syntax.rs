use owo_colors::colors::css::Orange;
use owo_colors::{CssColors, OwoColorize, Style};
use owo_format::format_args_colored;

macro_rules! format_colored {
    ( $($tt:tt)* ) => {
        ::std::fmt::format(format_args_colored!( $($tt)* ))
    };
}

// macro_rules! log {
//     ( $($tt:tt)* ) => {
//         print!("{}", format_colored!( $($tt)* ))
//     };
// }

macro_rules! logln {
    ( $($tt:tt)* ) => {
        println!("{}", format_colored!( $($tt)* ))
    };
}

fn main() {
    static EXAMPLE_DEBUG: Result<Option<usize>, ()> = Ok::<_, ()>(Some(usize::MAX));

    // Basic coloring
    logln!(:red "red text", :bold :blue "bold blue text");
    logln!(:bold :red :on_blue "red on blue text");

    // Custom coloring
    logln!(:bold :fg::<Orange> :bg_rgb::<33, 50, 12> "custom foreground on custom background");
    // format_args_colored!(:bold :color(Orange) :on_color((33, 50, 12)) "custom foreground on custom background");
    let hacker = Style::new().bright_green().bg_rgb::<0, 0, 0>().bold();
    logln!(:style(hacker) "hacker text");
    logln!(:color(CssColors::DeepSkyBlue) "CSS Deep Sky Blue");

    // Concatenation
    logln!("text" "concatenated" :red "without" "spaces");
    logln!(:bold "commas", "concatenate", :cyan "with", "spaces");
    logln!(:green "semicolons"; :red "will"; :underline :bold :cyan "break"; :magenta "lines");

    // Debug can be done as a string literal
    logln!("This is a debug format:", :purple "{EXAMPLE_DEBUG:?}");

    // Formatting can be escaped
    logln!(:cyan &"This is not {formatted}");

    // Bare identifiers should not work
    // This is to disambiguate from function identifiers.
    // format_args_colored!(:bright_red fail);

    // Identifiers should be in an expression (either inline or block) or a borrow.
    let ok = "identifier as expression or borrow";
    logln!((ok); {ok}; &ok);

    // Parenthesized items are expressions (not method arguments) because
    // they are either followed by punctuation or EOF.
    format_args_colored!(:green (ok));
    format_args_colored!(:green (ok), :green (ok));
    format_args_colored!(:green (ok) :blue (ok));
    // If they are punctuated with `Concat`, parentheses are ambiguous.
    // format_args_colored!(:green (ok) (ok) (ok));
    // format_args_colored!(:green() std::env::args());

    // Expressions don't need to have braces or parentheses.
    logln!(
        "These are expressions:";
        "Avagadro's number:", 6.02214076 * 10.0_f64.powi(23);
        "This value was unwrapped twice:", :black :on_bright_blue
            match EXAMPLE_DEBUG {
                Ok(Some(max)) => max - (u32::MAX as usize),
                _ => unreachable!("DEBUG is a static")
            }
    );

    // But they probably should.
    logln!(:bright_cyan "a", :bright_red "+", :bright_cyan "b", :yellow "=", {
        let (a, b) = (5, 5);
        a + b
    });

    // Now this macro supports recursion too!
    let style_paren = Style::new().dimmed().white().on_black();
    let style_single_quote = Style::new().yellow().on_blue();
    let style_text = Style::new().bright_cyan().on_purple();
    logln!(:style(style_paren) .{ "(" :style(style_single_quote) .{"'" :style(style_text) "text" "'"} ")"});
    logln!(:bright_red .{ "(" :green .{"'" :bold "quoted" "'"} ")" });
}
