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

- [ ] Style
- [ ] Nicer selection of sub reddits
- [ ] See if it makes sense to split this into separate files/modules
- [x] Type alias' and initial Model
- [x] Post Decoder
- [x] Posts Decoder
- [x] Http Cmd
- [x] Better view composition
- [x] Loading animations
- [x] Handle errors
- [x] Replace Json.Decoder.mapN with Json.Decode.Pipeline and Json.Decode.Extra
- [x] Conditionally show html based on fetching value
