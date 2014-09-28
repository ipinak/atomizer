
-module(rss_parser).

-export([parse_feed/1]).

-include("atomizer.hrl").

parse_feed(RawFeed) ->
	erlsom:sax(RawFeed, [], fun handle_event/2).

handle_event(startDocument, _State) ->
	[{cmd, start}, {md, #feed{}}, {entries, []}];

handle_event({endElement, _NS, "channel", _}, [{cmd, _Command}, {md, Feed}, {entries, Entries}]) ->
	Feed#feed{entries=lists:reverse(Entries)};

handle_event({startElement, _NS, "title", _, _Attrs}, [{cmd, start}, {md, Feed}, {entries, Entries}]) ->
	build_state(titletext, Feed, Entries);

handle_event({characters, Text}, [{cmd, titletext}, {md, Feed}, {entries, Entries}]) ->
	build_state(start, Feed#feed{title=Text}, Entries);

handle_event({startElement, _NS, "link", _, _Attrs}, [{cmd, start}, {md, Feed}, {entries, Entries}]) ->
	build_state(linktext, Feed, Entries);

handle_event({characters, Text}, [{cmd, linktext}, {md, Feed}, {entries, Entries}]) ->
	build_state(start, Feed#feed{url=Text}, Entries);

handle_event({startElement, _NS, "lastBuildDate", _, _Attrs}, [{cmd, start}, {md, Feed}, {entries, Entries}]) ->
	build_state(updatedtext, Feed, Entries);

handle_event({characters, Text}, [{cmd, updatedtext}, {md, Feed}, {entries, Entries}]) ->
	build_state(start, Feed#feed{updated=Text}, Entries);

handle_event({startElement, _NS, "item", _, _Attrs}, [{cmd, _Command}, {md, Feed}, {entries, Entries}]) ->
	build_state(entry, Feed, [#feedentry{content=""}|Entries]);

handle_event({endElement, _NS, "item", _}, [{cmd, _Command}, {md, Feed}, {entries, Entries}]) ->
	build_state(start, Feed, Entries);

handle_event({startElement, _NS, "title", _, _Attrs}, [{cmd, entry}, {md, Feed}, {entries, Entries}]) ->
	build_state(entrytitletext, Feed, Entries);

handle_event({characters, Text}, [{cmd, entrytitletext}, {md, Feed}, {entries, Entries}]) ->
	[Entry|T] = Entries,
	UpdatedEntry = Entry#feedentry{title=Text},
	build_state(entry, Feed, [UpdatedEntry|T]);

handle_event({startElement, _NS, "creator", _, _Attrs}, [{cmd, entry}, {md, Feed}, {entries, Entries}]) ->
	build_state(creatortext, Feed, Entries);

handle_event({characters, Text}, [{cmd, creatortext}, {md, Feed}, {entries, Entries}]) ->
 	[Entry|T] = Entries,
 	UpdatedEntry = Entry#feedentry{author=Text},
	build_state(entry, Feed, [UpdatedEntry|T]);

handle_event({startElement, _NS, "link", _, _Attrs}, [{cmd, entry}, {md, Feed}, {entries, Entries}]) ->
	build_state(entrylinktext, Feed, Entries);

handle_event({characters, Text}, [{cmd, entrylinktext}, {md, Feed}, {entries, Entries}]) ->
 	[Entry|T] = Entries,
 	UpdatedEntry = Entry#feedentry{permalink=Text},
	build_state(entry, Feed, [UpdatedEntry|T]);

handle_event({startElement, _NS, "description", _, _Attrs}, [{cmd, entry}, {md, Feed}, {entries, Entries}]) ->
	build_state(entrycontenttext, Feed, Entries);

handle_event({characters, Text}, [{cmd, entrycontenttext}, {md, Feed}, {entries, Entries}]) ->
 	[Entry|T] = Entries,
	UpdatedEntry = Entry#feedentry{content=lists:append(Entry#feedentry.content, Text)},
	UpdatedEntries = [UpdatedEntry|T],
	build_state(entry, Feed, UpdatedEntries);

handle_event({startElement, _NS, "pubDate", _, _Attrs}, [{cmd, entry}, {md, Feed}, {entries, Entries}]) ->
	build_state(pubdatetext, Feed, Entries);

handle_event({characters, Text}, [{cmd, pubdatetext}, {md, Feed}, {entries, Entries}]) ->
 	[Entry|T] = Entries,
 	UpdatedEntry = Entry#feedentry{date=Text},
	build_state(entry, Feed, [UpdatedEntry|T]);

handle_event(_Event, State) ->
	State.

build_state(Command, Feed, Entries) ->
	[{cmd, Command}, {md, Feed}, {entries, Entries}].
