import { Poppins } from "next/font/google";
import { useRouter } from "next/router";
import dynamic from "next/dynamic";
import { useContext, useEffect, useState } from "react";
import { CLUBS, FUNDRAISING_EVENTS } from "@/context/data";
import { WalletContext } from "@/context/wallet";
import { FundCard } from "@/components/FundCard";

const VideoMeeting = dynamic(() => import("@/components/Lounge"), {
  ssr: false,
});

const inter = Poppins({
  weight: ["100", "200", "300", "400", "500", "600", "700", "800", "900"],
  subsets: ["latin"],
});

export default function ClubView() {
  const { query, push } = useRouter();

  const clubData = CLUBS.find((club) => club.id === (query.club_id as string));

  const contextData = useContext(WalletContext);

  const [pageLoading, setPageLoading] = useState(true);

  useEffect(() => {
    if (!clubData) {
      void push("/");
      return;
    }
    if (
      !(
        contextData?.club_tokens &&
        contextData.club_tokens.find((club) => club.type === clubData.type)
      )
    ) {
      void push("/");
      return;
    }

    setPageLoading(false);
  }, [contextData, clubData]);

  if (pageLoading || !clubData) return <>Loading...</>;

  return (
    <main
    style={{
      background: `url(/${clubData.bg_img})`,
    }}
      className={`${inter.className} relative !bg-center !bg-cover !bg-fixed`}
    >
      <div className="absolute bg-black w-full h-full bg-opacity-80"></div>
      <div className="px-16 py-10  relative z-10">
        <div className="flex">
          <div className="w-[100%] m-auto">
            <VideoMeeting meetingId={clubData?.meet_id ?? ""} />
          </div>
        </div>
        <div className="flex gap-8">
          <div className="flex-1">
            <div className="bg-[#282828] mt-8 p-4 rounded-xl w-full">
              <div className="text-xl font-bold py-4">Upcoming events: </div>
              <div className="flex flex-col gap-4">
                {FUNDRAISING_EVENTS.filter(
                  (event) => event.id === query.club_id
                ).map((event) => (
                  <FundCard key={event.id} {...event} />
                ))}
              </div>
            </div>
          </div>
          <div className="flex-1 bg-[#282828] mt-8 p-4 rounded-xl flex flex-col gap-y-4 items-center justify-center w-full">
            <div className="text-xl font-bold w-full text-left">
              Upcoming events:{" "}
            </div>

            <div className="w-full m-auto h-[600px] rounded-xl overflow-hidden">
              <iframe
                src="https://teamup.com/kst7uv2esucjn9ifzu?showProfileAndInfo=0&showSidepanel=1&showAgendaHeader=1&showAgendaDetails=0&showYearViewHeader=1"
                style={{ width: "100%", height: "100%" }}
              ></iframe>
            </div>
          </div>
        </div>
      </div>
    </main>
  );
}
