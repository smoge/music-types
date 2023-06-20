# music-types

The code outlined here provides a starting point for building a music framework for music composition in Haskell.

This initial Haskell module (just a sketch for now) offers a set of types and functions for creating and managing basic musical data structures. The code provided in the "Rtm" module defines several data types and functions for building music compositions (especially rhythmic trees) in Haskell. 

## Quick tests:

```haskell
-- Example usage
main :: IO ()
main = do
  let input = "(1 -1 (1 (1 -1 1)))"
  let proportions = getRtmProportions input
  case proportions of
    Left err -> putStrLn $ "Parsing error: " ++ err
    Right rtm -> do
      putStrLn "Parsed RtmProportions:"
      putStrLn $ showRtmProportions rtm
      putStrLn "Formatted RtmProportions:"
      putStrLn $ formatRtmProportions 0 rtm
      let totalDuration = sumValues rtm
      putStrLn $ "Total duration: " ++ show totalDuration
```

## Contribution

This module serves as a starting point for building a more comprehensive music framework in Haskell. Contributions are welcome to extend the functionality, improve the parsing and formatting capabilities, and add additional features for music composition and manipulation. Feel free to fork the repository, make improvements, and submit pull requests.

## License

This project is licensed under the [GPL 3.0](https://www.gnu.org/licenses/gpl-3.0.en.html) license.
