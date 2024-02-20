export const authToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJhcGlrZXkiOiI5MzUzZWM3Mi0xNDIzLTQwMWYtOGU4Mi1kM2I3YTQ3MWI0NzgiLCJwZXJtaXNzaW9ucyI6WyJhbGxvd19qb2luIl0sImlhdCI6MTcwNzkwMTc3OCwiZXhwIjoxODY1Njg5Nzc4fQ.D31v_UeO37H0xCoJRNtO5sNBf2f5GwBbbdIvfNkjjYQ";

export const createMeeting = async ({ token }: { token: string }) => {
  const res = await fetch(`https://api.videosdk.live/v2/rooms`, {
    method: "POST",
    headers: {
      authorization: `${authToken}`,
      "Content-Type": "application/json",
    },
    body: JSON.stringify({}),
  });

  const { roomId } = await res.json();
  return roomId;
};
