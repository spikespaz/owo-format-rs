use owo_colors::OwoColorize;
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
    logln!(:red "red");
    logln!(:blue :on_red "blue", :fg_rgb::<255, 255, 255> "white" ; "bad" "ass");
    let interpolation = "be interpolated";
    logln!("concatenated" "text" 123 "stuff" {interpolation});
    logln!(
        :bold "bold", :black :on_white :italic "italic";
        "new line" "and" "no space";
        "words", "with", "spaces";
        :bright_purple "nested invocations",
        format_args_colored!(:bold :green "like this");
        "let interpolation", :bold :underline :cyan interpolation;
        :yellow "'" :italic "quoted text" :yellow "'"
    );
}
