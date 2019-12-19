import asyncio
import aiohttp
import json
import sys
import time
import iso6709


client_locations = {}
ports = {"Goloman" : 11770, "Hands" : 11771, "Holiday" : 11772, "Welsh" : 11773, "Wilkes" : 11774}
graph = {"Goloman" : ["Hands", "Holiday", "Wilkes"], "Hands" :  ["Wilkes", "Goloman"], "Holiday" : ["Welsh", "Wilkes", "Goloman"], "Wilkes" : ["Goloman", "Hands", "Holiday"], "Welsh": ["Holiday"]}


def log(server_identifier, message):
    with open("%s.txt" % server_identifier, "a") as log_file:
        log_file.write("%s\n" % message)


class Client(asyncio.Protocol):
    def __init__(self, server_identifier):
        self.server_identifier = server_identifier
        self.on_request_completed = None
        self.on_done_complete = None
        self.transport = None

    def request(self, message):
        log(self.server_identifier, "=> " + message);
        self.transport.write(message.encode())
        self.on_request_completed = asyncio.get_running_loop().create_future()
        return self.on_request_completed

    def done(self):
        self.transport.close()
        self.on_done_complete = asyncio.get_running_loop().create_future()
        return self.on_done_complete

    def connection_made(self, transport):
        self.transport = transport

    def data_received(self, data):
        message = data.decode()
        log(self.server_identifier, "<= " + message);
        self.on_request_completed.set_result(message)
        self.on_request_completed = None

    def connection_lost(self, exc):
        self.on_done_complete.set_result(True)
        self.on_done_complete = None
        self.transport = None


class ClientHandler(asyncio.Protocol):
    def __init__(self, server_identifier):
        self.server_identifier = server_identifier
        self.transport = None

    def connection_made(self, transport):
        self.transport = transport

    def data_received(self, data):
        message = data.decode()
        log(self.server_identifier, "<= " + message);
        tokens = message.split()
        succeeded = False
        if len(tokens) > 0 and tokens[0] == "IAMAT":
            if len(tokens) == 4:
                client_identifier = tokens[1]
                try:
                    client_location = iso6709.Location(tokens[2])
                except:
                    client_location = None
                try:
                    client_time = float(tokens[3])
                except:
                    client_time = None
                if client_location is not None and client_time is not None:
                    flood = client_identifier not in client_locations or client_locations[client_identifier] != tokens[2]
                    client_locations[client_identifier] = tokens[2]
                    response = "AT %s %s %s %s %s" % (self.server_identifier, time.time() - client_time, client_identifier, tokens[2], client_time)
                    log(self.server_identifier, "=> " + response);
                    self.transport.write(response.encode())
                    if flood:
                        asyncio.get_running_loop().create_task(self.flood(message))
                    succeeded = True
        elif len(tokens) > 0 and tokens[0] == "WHATSAT":
            if len(tokens) == 4:
                client_identifier = tokens[1]
                try:
                    radius = int(tokens[2])
                except:
                    radius = None
                try:
                    bound = int(tokens[3])
                except:
                    bound = None
                client_time = time.time()
                if radius is not None and bound is not None and client_identifier in client_locations:
                    client_location = client_locations[client_identifier]
                    prefix = "AT %s %s %s %s %s" % (self.server_identifier, time.time() - client_time, client_identifier, client_location, client_time)
                    longitude = iso6709.Location(client_location).lng.degrees
                    latitude = iso6709.Location(client_location).lat.degrees
                    asyncio.get_running_loop().create_task(self.query(prefix, "%s,%s" % (latitude, longitude), radius, bound))
                    succeeded = True
        if not succeeded:
            response = ("? " + message)
            log(self.server_identifier, "<= " + response);
            self.transport.write(response.encode())

    async def flood(self, message):
        for neighbor in graph[self.server_identifier]:
            try:
                _, client = await asyncio.get_running_loop().create_connection(lambda: Client(self.server_identifier), '127.0.0.1', ports[neighbor])
                _ = await client.request(message)
                await client.done()
            except:
                pass

    async def query(self, prefix, location, radius, bound):
        url = '{}location={}&radius={}&key={}'.format("https://maps.googleapis.com/maps/api/place/nearbysearch/json?", location, radius, "AIzaSyD8YjdG9ImRfntzSir2_iSsPXxII1ziloU")
        async with aiohttp.ClientSession() as session:
            async with session.get(url, ssl=False) as response:
                response_text = await response.text()
            deserialized_response = json.loads(response_text)
            deserialized_response['results'] = deserialized_response['results'][:bound]
            serialized_response = '%s\n{%s}\n\n' % (prefix, json.dumps(deserialized_response, indent=2))
            log(self.server_identifier, "=> " + serialized_response);
            self.transport.write(serialized_response.encode())


async def serve(server_identifier):
    server = await asyncio.get_running_loop().create_server(
        lambda: ClientHandler(server_identifier),
        '127.0.0.1', ports[server_identifier])
    async with server:
        await server.serve_forever()


def main(argv):
    if len(argv) == 2 and argv[1] in ports:
        asyncio.run(serve(argv[1]))
    else:
        print("Usage: [Golomon Hands Holiday Welsh Wilkes]")


if __name__ == "__main__":
    main(sys.argv)