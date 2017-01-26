## Reddit Client

This is a simple Reddit client written in elm.

###### Json Decoders

The main purpose of this project is to learn a little more about decoding complex, nested json structures in Elm. Here's what we're looking at from Reddit's api:

```
{
  "data": {
    "children": [
      {"data": {"url": "something.com", "title": "some title"}},
      {"data": {"url": "another-something.com", "title": "another title"}}
    ]
  }
}
```

###### Todo

- [x] Type alias' and initial Model
- [x] Post Decoder
- [x] Posts Decoder
- [x] Http Cmd
- [] Better view composition
- [] Handle errors
- [] Loading animations
- [] Nicer selection of sub reddits
- [] Style
