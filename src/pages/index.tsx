import React from "react";
import Head from "next/head";
import Image from "next/image";
import { Box, Button, HStack, useToast } from "@chakra-ui/react";
import { useEffect, useState } from "react";
import { NextPage } from "next";
import dynamic from "next/dynamic";
import axios from "axios";

const CodeEditor = dynamic(
  () => import("@/components/molecules/editor/CodeEditor"),
  { ssr: false }
);
const sampleCode = `
access(all) contract HelloWorld {
// Declare a public field of type String.
//
// All fields must be initialized in the init() function.
access(all) let greeting: String // The init() function is required if the contract contains any fields.
init(){self.greeting="Hello, World!"}
// Public function that returns our friendly greeting!
access(all) fun hello(): String {








return self.greeting
}
init(){self.greeting="Hello, World!"}
}
`;

const Home: NextPage = () => {
  const toast = useToast();
  const [before, setBefore] = useState<string>("");
  const [after, setAfter] = useState<string>("\n".repeat(29));

  const onClick = async () => {
    console.log(before);
    try {
      const res = await axios.post("/api/code", {
        code: before,
      });
      if (res.status === 200) {
        setAfter(res.data.code);
      } else {
        if (res.data.error) {
          toast({
            title: `Could not parse Code.`,
            status: "error",
            description:
              "We are currently expanding the grammars that can be supported. Please wait for a while.",
            isClosable: true,
          });
        }
        setAfter(res.data.error ?? "Unknown Error.");
      }
      console.log(res);
    } catch (error) {
      toast({
        title: `Network Error.`,
        status: "error",
        isClosable: true,
      });
      throw error;
    }
  };

  useEffect(() => {
    setBefore(sampleCode);
  }, []);

  return (
    <>
      <Head>
        <title>Create Next App</title>
        <meta name="description" content="Generated by create next app" />
        <meta name="viewport" content="width=device-width, initial-scale=1" />
        <link rel="icon" href="/favicon.ico" />
      </Head>
      <Box backgroundColor={"blackAlpha.700"} height="100vh">
        <Box height="5vh" />
        <HStack>
          <Box width={"5vw"} />
          <Image
            alt="logo"
            src={"/PrettifyCadence.png"}
            width="400"
            height="30"
          />
        </HStack>
        <HStack justifyContent={"center"} height={"80vh"}>
          <CodeEditor
            value={before}
            onChange={(value, event) => setBefore(value)}
            width={"40vw"}
            maxLines={40}
          />
          <Button ml="3vw" mr="3vw" type="button" onClick={() => onClick()}>
            {"Pretty >>"}
          </Button>
          <CodeEditor value={after} width={"40vw"} maxLines={40} />
        </HStack>
        <Box height="10vh" />
      </Box>
    </>
  );
};

export async function getStaticProps() {
  return {
    props: {},
  };
}

export default Home;
