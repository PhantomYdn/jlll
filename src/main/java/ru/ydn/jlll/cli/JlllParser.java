package ru.ydn.jlll.cli;

import java.util.ArrayList;
import java.util.List;
import org.jline.reader.CompletingParsedLine;
import org.jline.reader.ParsedLine;
import org.jline.reader.Parser;

/**
 * Lisp-aware parser for JLine that correctly identifies symbol boundaries.
 * Recognizes parentheses, brackets, quotes, and other Lisp delimiters as word terminators.
 */
public class JlllParser implements Parser
{
    @Override
    public ParsedLine parse(String line, int cursor, ParseContext context)
    {
        List<String> words = new ArrayList<>();
        List<Integer> wordPositions = new ArrayList<>();
        int wordStart = -1;
        boolean inString = false;
        boolean escape = false;
        int currentWordIndex = -1;
        int wordCursor = 0;
        for (int i = 0; i < line.length(); i++)
        {
            char c = line.charAt(i);
            if (escape)
            {
                escape = false;
                continue;
            }
            if (c == '\\' && inString)
            {
                escape = true;
                continue;
            }
            if (c == '"')
            {
                if (!inString)
                {
                    // Start of string - treat as a word boundary
                    if (wordStart >= 0)
                    {
                        words.add(line.substring(wordStart, i));
                        wordPositions.add(wordStart);
                        wordStart = -1;
                    }
                }
                inString = !inString;
                continue;
            }
            if (inString)
            {
                continue;
            }
            if (isSymbolTerminator(c))
            {
                // End current word if any
                if (wordStart >= 0)
                {
                    words.add(line.substring(wordStart, i));
                    wordPositions.add(wordStart);
                    wordStart = -1;
                }
            }
            else
            {
                // Start new word if not already in one
                if (wordStart < 0)
                {
                    wordStart = i;
                }
            }
        }
        // Handle word at end of line
        if (wordStart >= 0)
        {
            words.add(line.substring(wordStart));
            wordPositions.add(wordStart);
        }
        // Find which word the cursor is in (or at the end of)
        String currentWord = "";
        int currentWordStart = cursor;
        for (int i = 0; i < words.size(); i++)
        {
            int start = wordPositions.get(i);
            int end = start + words.get(i).length();
            if (cursor >= start && cursor <= end)
            {
                currentWordIndex = i;
                currentWord = words.get(i);
                currentWordStart = start;
                wordCursor = cursor - start;
                break;
            }
        }
        // If cursor is not in any word, check if we're starting a new word
        if (currentWordIndex < 0)
        {
            currentWordIndex = words.size();
            currentWord = "";
            currentWordStart = cursor;
            wordCursor = 0;
            // Look backwards to see if we should be continuing a word
            int pos = cursor - 1;
            while (pos >= 0 && !isSymbolTerminator(line.charAt(pos)) && line.charAt(pos) != '"')
            {
                pos--;
            }
            if (pos < cursor - 1)
            {
                currentWordStart = pos + 1;
                currentWord = line.substring(currentWordStart, cursor);
                wordCursor = cursor - currentWordStart;
            }
        }
        return new JlllParsedLine(line, cursor, words, currentWordIndex, currentWord, currentWordStart, wordCursor);
    }

    /**
     * Check if a character terminates a symbol.
     */
    private boolean isSymbolTerminator(char c)
    {
        return c == '(' || c == ')' || c == '[' || c == ']' || c == '\'' || c == '`' || c == ',' || c == '@' || c == '"'
                || c == ';' || Character.isWhitespace(c);
    }

    /**
     * Parsed line implementation for Lisp input.
     */
    private static class JlllParsedLine implements CompletingParsedLine
    {
        private final String line;
        private final int cursor;
        private final List<String> words;
        private final int wordIndex;
        private final String word;
        private final int wordStart;
        private final int wordCursor;

        JlllParsedLine(String line, int cursor, List<String> words, int wordIndex, String word, int wordStart,
                int wordCursor)
        {
            this.line = line;
            this.cursor = cursor;
            this.words = words;
            this.wordIndex = wordIndex;
            this.word = word;
            this.wordStart = wordStart;
            this.wordCursor = wordCursor;
        }

        @Override
        public String word()
        {
            return word;
        }

        @Override
        public int wordCursor()
        {
            return wordCursor;
        }

        @Override
        public int wordIndex()
        {
            return wordIndex;
        }

        @Override
        public List<String> words()
        {
            return words;
        }

        @Override
        public String line()
        {
            return line;
        }

        @Override
        public int cursor()
        {
            return cursor;
        }
        // CompletingParsedLine methods for proper completion insertion

        @Override
        public CharSequence escape(CharSequence candidate, boolean complete)
        {
            // No escaping needed for Lisp symbols
            return candidate;
        }

        @Override
        public int rawWordCursor()
        {
            return wordCursor;
        }

        @Override
        public int rawWordLength()
        {
            return word.length();
        }
    }
}
