import React, { FC, useRef } from "react";
import Head from "next/head";
import {
  Box,
  CircularProgress,
  HStack,
  Heading,
  Spacer,
  Tab,
  TabList,
  TabPanel,
  TabPanels,
  Tabs,
  VStack,
  useToast,
} from "@chakra-ui/react";
import { useEffect, useState } from "react";
import { NextPage } from "next";
import PasswordInput from "@/components/molecules/PasswordInput";
import DrawerMenu from "@/components/molecules/DrawerMenu";
import Conversion from "@/components/organisms/CodeCompletionConversion";
import { AxiosError } from "axios";

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
  const [apiKey, setApiKey] = useState<string>("");
  const [loading, setLoading] = useState<boolean>(false);
  const [error, setError] = useState<AxiosError | null>(null);

  useEffect(() => {
    if (error === null) return;
    toast({
      title: error.name,
      status: "error",
      description: error.message,
      isClosable: true,
    });
  }, [error, toast]);

  // call api
  return (
    <>
      <Head>
        <title>Code Completion🚀</title>
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
            Code Completion🚀
          </Heading>
        </HStack>
        <VStack height={"88%"} backgroundColor={"lightgrey"} m={"0 3vw"}>
          <PasswordInput
            onChange={(e) => setApiKey(e.target.value)}
            placeholder="Enter your OpenAI API apiKey."
            m={"10px 3vw"}
            h="50px"
            bgColor={"white"}
          />
          <Tabs size="md" variant="enclosed">
            <TabList>
              <Tab>Conversion</Tab>
              <Tab>Two</Tab>
            </TabList>
            <TabPanels>
              <TabPanel>
                <Conversion
                  init={sampleCode}
                  apiKey={apiKey}
                  setLoading={setLoading}
                  setError={setError}
                />
              </TabPanel>
              <TabPanel>
                <Conversion
                  init={sampleCode}
                  apiKey={apiKey}
                  setLoading={setLoading}
                  setError={setError}
                />
              </TabPanel>
            </TabPanels>
          </Tabs>
        </VStack>
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
