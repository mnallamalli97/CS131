import asyncio
import time


class Client(asyncio.Protocol):
    def __init__(self):
        self.on_request_completed = None
        self.on_done_complete = None
        self.transport = None

    def request(self, message):
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
        self.on_request_completed.set_result(data.decode())
        self.on_request_completed = None

    def connection_lost(self, exc):
        self.on_done_complete.set_result(True)
        self.on_done_complete = None
        self.transport = None


async def main():
    try:
        _, client = await asyncio.get_running_loop().create_connection(lambda: Client(), '127.0.0.1', 11770)
        print(await client.request("IAMAT kiwi.cs.ucla.edu +34.068930-118.445127 %s" % time.time()))
        print(await client.request("WHATSAT kiwi.cs.ucla.edu 10 5"))
        await client.done()
    except:
        print("Failed")


if __name__ == "__main__":
    asyncio.run(main())