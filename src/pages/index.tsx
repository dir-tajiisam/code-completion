import React from "react";
import Head from "next/head";
import Image from "next/image";
import {
  Box,
  Button,
  CircularProgress,
  HStack,
  Heading,
  Input,
  MenuIcon,
  MenuItem,
  MenuList,
  Select,
  VStack,
  useToast,
} from "@chakra-ui/react";
import { useEffect, useState } from "react";
import { NextPage } from "next";
import dynamic from "next/dynamic";
import axios, { AxiosError, AxiosResponse } from "axios";
import { useApi } from "@/hooks/openai-api";
import PasswordInput from "@/components/molecules/PasswordInput";

const CodeEditor = dynamic(
  () => import("@/components/molecules/editor/CodeEditor"),
  { ssr: false }
);
const sampleCode = `IDENTIFICATION DIVISION.
PROGRAM-ID. ORDER-DISCOUNT.

DATA DIVISION.
WORKING-STORAGE SECTION.
01 COMPANY.
   05 COMPANY-NAME	  PIC X(60)    VALUE "Semantic Designs".
   05 COMPANY-ADDRESS.
      10 STREET		  PIC X(80)    VALUE "8101 Asmara Dr.".
      10 CITY.
   15 CITY-NAME	  PIC X(40)	   VALUE "Austin".
   15 FILLER	  PIC XX	   VALUE ", ".
   15 CITY-STATE	  PIC XX	   VALUE "TX".
   15 ZIP.
      20 ZIP-5	  PIC 9(5)	   VALUE 78750.
01 LINE-ITEM.
   05 ITEM		  PIC X(20)	   VALUE "Blue widget".
   05 QUANTITY		  PIC 999	   VALUE 217.
   05 PRICE		  PIC 9999V99	   VALUE 24.95.

77 TOTAL-AMOUNT		  PIC 999999V99.
77 DISCOUNT-THRESHOLD	  PIC 999999V99	   VALUE 1111.11.
77 DISCOUNT-PERCENT	  PIC 99	   VALUE 20.
77 DISCOUNT-AMOUNT	  PIC 99999999V99.
77 TOTAL-FOR-OUTPUT	  PIC $$$$$$9.99.

PROCEDURE DIVISION.
PERFORM-TASK.
  PERFORM COMPUTE-TOTAL.
  PERFORM DISPLAY-TOTAL.
  STOP RUN.

COMPUTE-TOTAL.
  MULTIPLY QUANTITY BY PRICE GIVING TOTAL-AMOUNT.
  IF TOTAL-AMOUNT > DISCOUNT-THRESHOLD
    MULTIPLY TOTAL-AMOUNT BY DISCOUNT-PERCENT
        GIVING DISCOUNT-AMOUNT
    DIVIDE 100 INTO DISCOUNT-AMOUNT
    SUBTRACT DISCOUNT-AMOUNT FROM TOTAL-AMOUNT.

DISPLAY-TOTAL.
  DISPLAY COMPANY-NAME.
  MOVE TOTAL-AMOUNT TO TOTAL-FOR-OUTPUT.
  DISPLAY "Total: ", TOTAL-FOR-OUTPUT.
`;
const Home: NextPage = () => {
  const toast = useToast();
  const [before, setBefore] = useState<string>("");
  const [after, setAfter] = useState<string>("\n".repeat(29));
  const [mode, setMode] = useState<string>("");
  const [key, setKey] = useState<string>("");
  const [canCallApi, setCanCallApi] = useState<boolean>(false);

  // call api
  const { response, loading, error, valid, handleSubmit } = useApi();
  const onClick = async () => {
    handleSubmit({ code: before, key: key, mode: mode });
  };

  useEffect(() => {
    setBefore(sampleCode);
  }, []);

  useEffect(() => {
    setCanCallApi(before !== "" && mode !== "" && key !== "");
  }, [before, mode, key]);

  return (
    <>
      <Head>
        <title>Conversion</title>
      </Head>
      <Box height="100vh">
        {loading && (
          <Box
            position={"fixed"}
            top={0}
            left={0}
            width={"100vw"}
            height={"100vh"}
            backgroundColor={"blackAlpha.700"}
            display={"flex"}
            justifyContent={"center"}
            alignItems={"center"}
            zIndex={1000}
            backdropBlur={"lg"}
          >
            <CircularProgress
              isIndeterminate
              size={"100px"}
              thickness="6px"
              color="pink.400"
            />
            <Heading as={"h2"} color={"white"} ml={4}>
              AI Thinking...
            </Heading>
          </Box>
        )}
        <HStack>
          <Heading m="3vh 3vw" variant={"h1"}>
            Code Conversion
          </Heading>
        </HStack>
        <VStack height={"85%"} backgroundColor={"lightgrey"} m={"0 3vw"}>
          <PasswordInput
            onChange={(e) => setKey(e.target.value)}
            placeholder="Enter your OpenAI API Key."
            m={"10px 3vw"}
            h="50px"
            bgColor={"white"}
          />
          <HStack justifyContent={"center"}>
            <CodeEditor
              value={before}
              onChange={(value, event) => setBefore(value)}
              width={"38vw"}
              maxLines={45}
              mode="javascript"
            />
            <VStack width={"10vw"}>
              <Button
                ml="3vw"
                mr="3vw"
                type="button"
                onClick={() => onClick()}
                shadow={"lg"}
                width={"100%"}
                bgColor={"pink.200"}
                isDisabled={!canCallApi}
              >
                {"Conversion!!"}
              </Button>
              <Select
                textAlign={"center"}
                variant={"outline"}
                placeholder="Language"
                shadow={"lg"}
                bgColor={"pink.200"}
                fontWeight={"bold"}
                onChange={(e) => setMode(e.target.value)}
              >
                <option value="JAVA">JAVA</option>
                <option value="Python">Python</option>
                <option value="COBOL">COBOL</option>
              </Select>
            </VStack>
            <CodeEditor
              value={response.code}
              width={"38vw"}
              maxLines={45}
              mode={response.mode}
            />
          </HStack>
        </VStack>
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
