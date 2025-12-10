implement main
    open core

class predicates
    checkWords : (string, integer).

class facts
    output : string := "".

clauses
    run() :-
        stdio::write("Enter minimum word length: "),
        NStr = stdio::readLine(),
        N = toTerm(integer, NStr),
        output := "",
        FileName = "text.txt",
        Text = file::readString(FileName),
        Words = string::split(Text, " \n\t\r"),
        foreach Word = list::getMember_nd(Words) do
            if Word <> "" then
                checkWords(Word, N)
            end if
        end foreach,
        stdio::write(output).

    checkWords(Word, N) :-
        LowerWord = string::toLowerCase(Word),
        if string::length(Word) > N
            and (string::hasPrefix(LowerWord, "a")
                or string::hasPrefix(LowerWord, "e")
                or string::hasPrefix(LowerWord, "i")
                or string::hasPrefix(LowerWord, "o")
                or string::hasPrefix(LowerWord, "u") %KISS
                )
        then
            output := string::concat(output, " { ", Word, " } ")
        else
            output := string::concat(output, Word, " ")
        end if.

end implement main

goal
    console::runUtf8(main::run).
